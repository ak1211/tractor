{-
    This file is part of Tractor.

    Tractor is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Tractor is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with Tractor.  If not, see <http://www.gnu.org/licenses/>.
-}
{- |
Module      :  StockQuotesCrawler
Description :  
Copyright   :  (c) 2017 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  POSIX

WEBスクレイピングしてきた株式情報を
MariaDBデータベースに入れる
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
module StockQuotesCrawler
    (runWebCrawlingOfPortfolios
    ) where

import Control.Exception hiding (throw)
import Control.Applicative ((<|>))

import Text.XML.Cursor (($//), (&/))
import qualified Text.XML.Cursor as X
import qualified Text.XML as X
import qualified Text.HTML.DOM as H

import qualified Data.List as List
import qualified Data.Maybe as Mb
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import Data.Monoid ((<>))

import qualified Network.URI as N
import qualified Network.HTTP.Conduit as N

import qualified Control.Monad as M
import qualified Control.Monad.IO.Class as M

import qualified Control.Monad.Trans.Resource as MT
import Control.Monad.Logger
import Database.Persist ((=.), (==.), (<=.), (||.))
import qualified Database.Persist as DB
import qualified Database.Persist.Sql as DB
import qualified Database.Persist.MySQL as MySQL
import Data.Maybe (listToMaybe)
import qualified Data.Time.Clock as Tm
import qualified Data.Conduit as C
import qualified Control.Concurrent as CC

import qualified WebBot as WB
import TickerSymbol as Import
import TimeFrame as Import
import Model
import Conf
import qualified Lib

-- | スクリーンスクレイピング関数(k-db.com用)
kdbcomScreenScraper   :: TickerSymbol
                        -> TimeFrame
                        -> Maybe T.Text
                        -> BL.ByteString
                        -> Either String (Maybe T.Text, [Ohlcvt])
kdbcomScreenScraper ticker tf sourceName html = do
    let doc = X.fromDocument $ H.parseLBS html
    vs <- mapM toOHLCVT $ records doc
    Right (caption doc, vs)
    where
    --
    rational :: Maybe T.Text -> Maybe Double
    rational Nothing = Nothing
    rational (Just t) =
        either (const Nothing) (Just . fst) $ T.rational t
    -- | ohlcvtテーブルに変換する関数
    toOHLCVT :: [Maybe T.Text] -> Either String Ohlcvt
    toOHLCVT (d : tm : vs) = do
        d' <- maybe (Left "Date field can't parsed.") (Right . T.unpack) d
        let at = Lib.parseJSTDayTimeToUTCTime d' $ fmap T.unpack tm
        case take 6 $ init vs of  -- 最終要素は未使用
            ohlcvt@[_,_,_,_,_,_] -> Right $
                -- 個別株は6本値
                let [o, h, l, c, v, t] = map rational ohlcvt in
                Ohlcvt  { ohlcvtTicker      = ticker
                        , ohlcvtTf          = tf
                        , ohlcvtAt          = at
                        , ohlcvtOpen        = o
                        , ohlcvtHigh        = h
                        , ohlcvtLow         = l
                        , ohlcvtClose       = c
                        , ohlcvtVolume      = v
                        , ohlcvtTurnover    = t
                        , ohlcvtSource      = sourceName
                        }
            ohlc@[_,_,_,_] -> Right $
                -- 指数は4本値
                let [o, h, l, c] = map rational ohlc in
                Ohlcvt  { ohlcvtTicker      = ticker
                        , ohlcvtTf          = tf
                        , ohlcvtAt          = at
                        , ohlcvtOpen        = o
                        , ohlcvtHigh        = h
                        , ohlcvtLow         = l
                        , ohlcvtClose       = c
                        , ohlcvtVolume      = Nothing
                        , ohlcvtTurnover    = Nothing
                        , ohlcvtSource      = sourceName
                        }
            _ ->
                Left "ohlcvt table can't parsed"
    -- 株価情報ではない何かが表示されている場合
    toOHLCVT _ = Left "This web page can't parsed"
    -- | 銘柄名のXPath ---> //*[@id="tablecaption"]
    caption :: X.Cursor -> Maybe T.Text
    caption doc = listToMaybe (doc$//X.attributeIs "id" "tablecaption"&/X.content)
    -- | 4,6本値のXPath ---> //*[@id="maintable"]/tbody/tr
    records :: X.Cursor -> [[Maybe T.Text]]
    records doc =
        let tr = doc$//X.attributeIs "id" "maintable"&/X.element "tbody"&/X.element "tr" in
        [   [ listToMaybe                                       -- Maybeで得る
                [ content                                       -- contentの内容を
                | (X.NodeContent content)<-X.elementNodes e' ]  -- content要素を取り出して
            | (X.NodeElement e')<-X.elementNodes e ]            -- 要素を取り出して
        | (X.NodeElement e)<-map X.node tr ]                    -- trの子の

-- | インターネット上の株価情報を取りに行く関数
fetchStockPrices :: Conf.Info -> TickerSymbol -> TimeFrame -> IO (Maybe T.Text, [Ohlcvt])
fetchStockPrices conf ticker tf = do
    -- HTTPリクエストヘッダ
    let customHeader = Lib.httpRequestHeader conf
    let aUri = accessURI ticker
    manager <- N.newManager N.tlsManagerSettings
    uri <- maybe (throwIO $ userError "access uri parse error") pure $ N.parseURIReference aUri
    response <- WB.fetchPage manager customHeader Nothing [] uri
    --
    let body = N.responseBody response
    either (throwIO . userError) pure $
        kdbcomScreenScraper ticker tf (Just $ T.pack aUri) body
    where
    -- | アクセスURI
    accessURI :: TickerSymbol -> String
    accessURI symbol =
        "http://k-db.com/"
        ++ case symbol of
            -- 東証:個別株
            TSTYO c     -> "stocks/" ++ show c ++  "-T/"
            -- 日経平均株価
            TSNI225     -> "indices/I101/"
            -- TOPIX
            TSTOPIX     -> "indices/I102/"
            -- JPX日経インデックス400
            TSJPXNI400  -> "indices/I103/"
        ++ case tf of
            TF1h -> "1h"
            TF1d -> ""      -- 何もつけないのが日足

-- | ポートフォリオの株価情報を取りに行く関数
runWebCrawlingOfPortfolios :: M.MonadIO m => Conf.Info -> C.Source m TL.Text
runWebCrawlingOfPortfolios conf = do
    let deltaT = fromInteger (12*60*60)  -- 12時間
    limitTm <- M.liftIO $ pure . Tm.addUTCTime (-1 * deltaT) =<< Tm.getCurrentTime

    counts <- M.liftIO . runStderrLoggingT . MT.runResourceT . MySQL.withMySQLConn connInfo . MySQL.runSqlConn $ do
        DB.runMigration migrateQuotes
        DB.count ([PortfolioUpdateAt ==. Nothing] ||. [PortfolioUpdateAt <=. Just limitTm])
    --
    C.yield . TB.toLazyText $ case counts of
                    0 -> "今回の更新は不要です。"
                    x -> "更新対象は全部で" <> TB.decimal x  <> "個有ります。"

    -- 前回のアクセスからdeltaT時間以上経過した物のリストを得る
    ws <- M.liftIO . runStderrLoggingT . MT.runResourceT . MySQL.withMySQLConn connInfo . MySQL.runSqlConn $ do
        DB.runMigration migrateQuotes
        DB.selectList ([PortfolioUpdateAt ==. Nothing] ||. [PortfolioUpdateAt <=. Just limitTm]) []

    -- 更新アクション
    let actions = concatMap
                    (\wsnb ->
                        case portfolioUpdateAt . DB.entityVal . fst $ wsnb of
                        -- Nothing＝初取得の場合は日足も取得する
                        Nothing ->
                            [ updateTimeAndSales counts TF1d wsnb   -- 日足の取得
                            , updateTimeAndSales counts TF1h wsnb   -- 1時間足の取得
                            ]
                        -- 通常は１時間足のみ取得する
                        Just _ ->
                            [updateTimeAndSales counts TF1h wsnb]   -- 1時間足の取得
                    )
                    $ zip ws [1..]

    -- 271秒(4分31秒)の停止アクション
    let waitAction = M.liftIO $ CC.threadDelay (271 * 1000 * 1000)

    -- 更新作業毎に停止しながら更新する
    M.sequence_ $ List.intersperse waitAction actions

    C.yield "以上。更新処理を終了します。"
    where
    --
    connInfo :: MySQL.ConnectInfo
    connInfo =
        let mdb = Conf.mariaDB conf in
        MySQL.defaultConnectInfo
            { MySQL.connectHost = Conf.host mdb
            , MySQL.connectPort = Conf.port mdb
            , MySQL.connectUser = Conf.user mdb
            , MySQL.connectPassword = Conf.password mdb
            , MySQL.connectDatabase = Conf.database mdb
            }
    --
    updateTimeAndSales  :: M.MonadIO m
                        => Int
                        -> TimeFrame
                        -> (DB.Entity Portfolio, Int)
                        -> C.Source m TL.Text
    updateTimeAndSales total tf (DB.Entity wKey wVal, number) = do
        let textCaption = Mb.fromMaybe (T.pack . show . portfolioTicker $ wVal) $ portfolioCaption wVal
        C.yield . TB.toLazyText $
                    TB.fromText textCaption
                    <> " の"
                    <> case tf of
                        TF1h -> "１時間足"
                        TF1d -> "日足"
                    <> "を更新中。"
                    <> TB.singleton '['
                    <> TB.decimal number
                    <> TB.singleton '/'
                    <> TB.decimal total
                    <> TB.singleton ']'
        M.liftIO . runStderrLoggingT . MT.runResourceT . MySQL.withMySQLConn connInfo . MySQL.runSqlConn $ do
            DB.runMigration migrateQuotes
            -- インターネットから株価情報を取得する
            (caption, ohlcvts) <- M.liftIO $ fetchStockPrices conf (portfolioTicker wVal) tf
            updateAt <- M.liftIO Tm.getCurrentTime
            -- 6本値をohlcvtテーブルに書き込む
            M.forM_ ohlcvts $ \val ->
                -- 同じ時間の物があるか探してみる
                DB.selectFirst  [ OhlcvtTicker ==. ohlcvtTicker val
                                , OhlcvtTf ==. ohlcvtTf val
                                , OhlcvtAt ==. ohlcvtAt val] []
                >>= \case
                -- レコードが無かった場合は新規挿入する
                Nothing -> M.void $ DB.insert val
                -- レコードが有った場合は6本値とソース元だけ更新する
                Just (DB.Entity updKey _) ->
                    DB.update updKey
                        [ OhlcvtOpen    =. ohlcvtOpen val
                        , OhlcvtHigh    =. ohlcvtHigh val
                        , OhlcvtLow     =. ohlcvtLow val
                        , OhlcvtClose   =. ohlcvtClose val
                        , OhlcvtVolume  =. ohlcvtVolume val
                        , OhlcvtTurnover=. ohlcvtTurnover val
                        , OhlcvtSource  =. ohlcvtSource val
                        ]
            -- 更新日等をPortfolioテーブルに
            DB.update wKey
                [ PortfolioCaption  =. (portfolioCaption wVal <|> caption)  -- 以前から有るのを優先的に選択する
                , PortfolioUpdateAt =. Just updateAt
                ]


