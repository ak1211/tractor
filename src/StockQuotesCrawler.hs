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
Copyright   :  (c) 2016 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  POSIX

WEBスクレイピングしてきた株式情報を
MariaDBデータベースに入れる
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
module StockQuotesCrawler
    ( kdbcomScreenScraper
    , runWebCrawlingPortfolios
    ) where

import           Control.Applicative          ((<|>))
import qualified Control.Concurrent           as CC
import           Control.Exception.Safe
import qualified Control.Monad                as M
import qualified Control.Monad.IO.Class       as M
import qualified Control.Monad.Logger         as ML
import qualified Control.Monad.Trans.Resource as MR
import qualified Data.ByteString.Lazy         as BL
import qualified Data.Conduit                 as C
import qualified Data.List                    as List
import           Data.Maybe                   (listToMaybe)
import qualified Data.Maybe                   as Mb
import           Data.Monoid                  ((<>))
import qualified Data.Text                    as T
import qualified Data.Text.Lazy               as TL
import qualified Data.Text.Lazy.Builder       as TLB
import qualified Data.Text.Lazy.Builder.Int   as TLB
import qualified Data.Text.Read               as T
import qualified Data.Time.Clock              as Tm
import           Database.Persist             ((<=.), (=.), (==.), (||.))
import qualified Database.Persist             as DB
import qualified Database.Persist.MySQL       as MySQL
import qualified Database.Persist.Sql         as DB
import qualified Network.HTTP.Conduit         as N
import qualified Network.URI                  as N
import qualified System.Random.MWC            as R
import qualified Text.HTML.DOM                as H
import qualified Text.XML                     as X
import           Text.XML.Cursor              (($//), (&/))
import qualified Text.XML.Cursor              as X

import qualified BrokerBackend                as BB
import           Conf
import qualified Lib
import           Model

-- |
-- スクリーンスクレイピング関数(k-db.com用)
kdbcomScreenScraper :: TickerSymbol
                    -> TimeFrame
                    -> Maybe T.Text
                    -> BL.ByteString
                    -> Either String (Maybe T.Text, [Ohlcvt])
kdbcomScreenScraper ticker tf sourceName html = do
    vs <- mapM packOHLCVT $ records xdoc
    Right (caption xdoc, vs)
    where
    --
    --
    xdoc = X.fromDocument $ H.parseLBS html
    --
    --
    rational :: Maybe T.Text -> Maybe Double
    rational Nothing = Nothing
    rational (Just t) =
        either (const Nothing) (Just . fst) $ T.rational t
    -- |
    -- ohlcvtテーブルに変換する関数
    packOHLCVT :: [Maybe T.Text] -> Either String Ohlcvt
    packOHLCVT (d : tm : vs) = do
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
    --
    -- 株価情報ではない何かが表示されている場合
    packOHLCVT _ = Left "This web page can't parsed"
    -- |
    -- 銘柄名のXPath ---> //*[@id="tablecaption"]
    caption :: X.Cursor -> Maybe T.Text
    caption doc = listToMaybe (doc$//X.attributeIs "id" "tablecaption"&/X.content)
    -- |
    -- 4,6本値のXPath ---> //*[@id="maintable"]/tbody/tr
    records :: X.Cursor -> [[Maybe T.Text]]
    records doc =
        let tr = doc$//X.attributeIs "id" "maintable"&/X.element "tbody"&/X.element "tr" in
        [   [ listToMaybe                                       -- Maybeで得る
                [ content                                       -- contentの内容を
                | (X.NodeContent content)<-X.elementNodes e' ]  -- content要素を取り出して
            | (X.NodeElement e')<-X.elementNodes e ]            -- 要素を取り出して
        | (X.NodeElement e)<-map X.node tr ]                    -- trの子の

-- |
-- インターネット上の株価情報を取りに行く関数
fetchStockPrices :: Conf.Info -> TickerSymbol -> TimeFrame -> IO (Maybe T.Text, [Ohlcvt])
fetchStockPrices conf ticker tf = do
    let aUri = accessURI ticker
    manager <- N.newManager N.tlsManagerSettings
    uri <- maybe (throwString "access uri parse error") pure $ N.parseURIReference aUri
    response <- BB.fetchHTTP manager customHeader Nothing [] uri
    --
    let body = N.responseBody response
    either throwString pure $
        kdbcomScreenScraper ticker tf (Just $ T.pack aUri) body
    where
    -- |
    -- HTTPリクエストヘッダ
    customHeader = Lib.httpRequestHeader $ Conf.userAgent (conf::Conf.Info)
    -- |
    -- アクセスURI
    accessURI :: TickerSymbol -> String
    accessURI symbol =
        "http://k-db.com/"
        ++ case symbol of
            -- 東証:個別株
            TSTYO c    -> "stocks/" ++ show c ++  "-T/"
            -- 日経平均株価
            TSNI225    -> "indices/I101/"
            -- TOPIX
            TSTOPIX    -> "indices/I102/"
            -- JPX日経インデックス400
            TSJPXNI400 -> "indices/I103/"
        ++ case tf of
            TF1h -> "1h"
            TF1d -> ""      -- 何もつけないのが日足


-- |
-- ポートフォリオの株価情報を取りに行く関数
runWebCrawlingPortfolios :: M.MonadIO m => Conf.Info -> C.Source m TL.Text
runWebCrawlingPortfolios conf = do
    limitTm <- M.liftIO $ diffTime <$> Tm.getCurrentTime
    -- 前回の更新から一定時間以上経過した更新対象のリストを得る
    ws <- M.liftIO . ML.runStderrLoggingT . MR.runResourceT . MySQL.withMySQLConn connInfo . MySQL.runSqlConn $ do
        DB.runMigration migrateQuotes
        DB.selectList ([PortfolioUpdateAt ==. Nothing] ||. [PortfolioUpdateAt <=. Just limitTm]) []

    case length ws of
        0 ->
            C.yield . TLB.toLazyText $ "今回の更新は不要です。"
        counts -> do
            C.yield . TLB.toLazyText $ "更新対象は全部で" <> TLB.decimal counts  <> "個有ります。"
            rgen <- M.liftIO R.createSystemRandom
            -- 更新作業毎に停止しながら更新する
            M.sequence_
                -- アクセス毎の停止アクションを挟み込む
                . List.intersperse (randomWait rgen)
                -- 更新アクションのリスト
                . concatMap updateActs $ Lib.packNthOfTotal ws
            C.yield "以上で更新処理を終了します。"
    where
    --
    -- (-12)時間の足し算は12時間の引き算になる
    diffTime :: Tm.UTCTime -> Tm.UTCTime
    diffTime = Tm.addUTCTime $ fromInteger (-12*60*60)
    --
    -- アクセス毎の停止アクション
    randomWait rgen = M.liftIO $
        -- 241秒(4分01秒)から277秒(4分37秒)までの停止アクション
        CC.threadDelay =<< R.uniformR (lowerBound,upperBound) rgen
        where
        lowerBound = 241*1000*1000
        upperBound = 277*1000*1000
    --
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
    --
    updateActs  :: M.MonadIO m
                => (DB.Entity Portfolio, Lib.NthOfTotal)
                -> [C.Source m TL.Text]
    updateActs (pf,nth) =
        -- 初取得(Nothing)の場合はfirstUpdate
        maybe firstUpdate nextUpdate updateAt
        where
        --
        updateAt = portfolioUpdateAt . DB.entityVal $ pf
        -- 初取得の場合は日足と1時間足の両方を取得する
        firstUpdate = [act TF1d pf, act TF1h pf]
        -- 通常は１時間足のみ取得する
        nextUpdate = const [act TF1h pf]
        --
        act tf e@(DB.Entity _ v) = do
            putDescription tf v nth     -- 更新処理対象銘柄を説明する
            updateTimeAndSales tf e     -- 更新処理本体
    --
    --
    updateTimeAndSales  :: M.MonadIO m
                        => TimeFrame
                        -> DB.Entity Portfolio
                        -> C.Source m TL.Text
    updateTimeAndSales tf (DB.Entity wKey wVal) =
        M.liftIO . ML.runStderrLoggingT . MR.runResourceT . MySQL.withMySQLConn connInfo . MySQL.runSqlConn $ do
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
                [ PortfolioCaption  =. (portfolioCaption wVal <|> caption)  -- 以前から有る銘柄名を優先的に選択する
                , PortfolioUpdateAt =. Just updateAt
                ]

-- |
-- 更新銘柄の説明を送る関数
putDescription  :: M.MonadIO m
                => TimeFrame
                -> Portfolio
                -> Lib.NthOfTotal
                -> C.Source m TL.Text
putDescription tf pf (current,total) =
    C.yield $ TLB.toLazyText msg
    where
    defaultCaption = T.pack . show . portfolioTicker $ pf
    textCaption = Mb.fromMaybe defaultCaption $ portfolioCaption pf
    c = TLB.fromText textCaption
    t TF1h = "１時間足"
    t TF1d = "日足"
    s = TLB.singleton
    d = TLB.decimal
    body = c <> " の" <> t tf <> "を更新中。"
    trailer = s '[' <> d current <> s '/' <> d total <> s ']'
    msg = body <> trailer

