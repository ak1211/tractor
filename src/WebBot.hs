{- |
Module      :  WebBot.hs
Description :  Crawl websites
Copyright   :  (c) 2016, 2017 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  portable

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
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module WebBot
    ( UnexpectedHTMLException
    , HTTPSession (..)
    , Loghttp (..)
    , fetchPage
    , login
    , logout
    , clickLinkText
    , fetchFraHomeAnnounce
    , fetchFraStkSell
    , fetchFraAstSpare
    ) where

import qualified Scraper
import qualified DataBase

import Debug.Trace (trace)
import Prelude hiding (catch)
import Control.Exception hiding (throw)
import Safe
import qualified Conf

--import qualified Control.Applicative
import Control.Applicative ((<$>), (<*>))
import qualified Control.Monad as M
import qualified Control.Monad.IO.Class as M
import qualified Control.Monad.Reader as M
import Control.Monad.Trans.Resource (runResourceT)

import Data.Conduit (Source, ($$), yield)
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL

import qualified Data.Either as Either
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.List.Split as List.Split
import qualified Data.Char
import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text.Lazy.Read as Read
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as TE
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.Encoding.Error as Encoding.Error
import qualified Data.Typeable
import qualified Data.Maybe

import qualified Codec.Text.IConv as IConv
import qualified Text.Printf as Printf

import qualified Network.Connection as N
import qualified Network.URI as N
import qualified Network.HTTP.Conduit as N
import qualified Network.HTTP.Types.Header as N
import qualified Network.HTTP.Types.Method as N

import Database.Persist ((==.), (!=.), (>.), (<=.))
import qualified Database.Persist as DB
import qualified Database.Persist.TH as DB
import qualified Database.Persist.Sql as DB
import qualified Database.Persist.Sqlite as Sqlite

import Text.HTML.TagSoup ((~==), (~/=))
import qualified Text.HTML.TagSoup as TS
import qualified Text.HTML.TagSoup.Tree as TS
import qualified Text.HTML.TagSoup.Match as TS

import Data.Time (Day(..), TimeOfDay, UTCTime, parseTime, getCurrentTime)

{-
 - 実行時例外 : 予想外のHTML
 -}
data UnexpectedHTMLException = UnexpectedHTMLException String
    deriving (Data.Typeable.Typeable, Eq)

instance Show UnexpectedHTMLException where
    show (UnexpectedHTMLException s) = s

instance Exception UnexpectedHTMLException

{-
 - 実行時例外 : 未保有株の売却指示
 -}
data DontHaveStocksToSellException = DontHaveStocksToSellException String
    deriving (Data.Typeable.Typeable, Eq, Show)

instance Exception DontHaveStocksToSellException

{-
 - セッション情報
 -}
data HTTPSession = HTTPSession {
    sLoginPageURI   :: N.URI,
    sManager        :: N.Manager,
    sReqHeaders     :: N.RequestHeaders,
    sRespCookies    :: N.CookieJar,
    sTopPageHTML    :: T.Text
}

instance Show HTTPSession where
    show (HTTPSession a b c d e) =
        Printf.printf "%s, %s, %s, %s" (show a) (show c) (show d) (show e)

{-
 - Debug.trace関数をはさむ関数
 -}
withTrace :: (Show str, Show a) => str -> a -> a
withTrace msg x = trace (show msg ++ " : " ++ show x) x

{-
 - HTTP通信記録用データーベース
 - urlのページにHTTP通信をした時の返答
 -}
DB.share [DB.mkPersist DB.sqlSettings, DB.mkMigrate "migrateAll"] [DB.persistLowerCase|
Loghttp
    url             String      -- ページのURL
    scheme          String      -- スキーム
    userInfo        String      -- オーソリティ
    host            String      -- オーソリティ
    port            String      -- オーソリティ
    path            String      -- パス
    query           String      -- クエリ
    fragment        String      -- フラグメント
    --
    reqMethod       String      -- 要求メソッド(GET / POST)
    reqHeader       String      -- 要求ヘッダ
    reqCookie       String      -- 要求クッキー
    --
    respDatetime    UTCTime     -- 返答時間
    respStatus      String      -- 返答HTTP status code
    respVersion     String      -- 返答HTTP version
    respHeader      String      -- 返答ヘッダ
    respCookie      String      -- 返答クッキー
    respBody        ByteString  -- 返答ボディ(HTML)
    deriving Show
|]

{-
 - ログをデーターベースへ格納する
 -}
storeLogDB :: (Monad m, M.MonadIO m) => Loghttp -> m (DB.Key Loghttp)
storeLogDB log = M.liftIO $
    Sqlite.runSqlite "loghttp.sqlite3" $ do
        Sqlite.runMigration migrateAll
        DB.insert log

{-
 - urlに接続して結果を得るついでにDBに記録する
 -}
fetchPage :: M.MonadIO m => N.Manager
            -> N.RequestHeaders -> Maybe N.CookieJar -> [(B8.ByteString, B8.ByteString)]
            -> N.URI -> m (N.Response BL8.ByteString)
fetchPage manager header cookie reqBody url = M.liftIO $
    -- HTTPリクエストを作る
    N.parseRequest (show url)
    -- HTTPヘッダを指定する
    >>= \r -> return (r { N.cookieJar = cookie
                        , N.requestHeaders = header
                        })
    -- HTTPリクエストボディを指定する
    >>= \r -> case reqBody of
                [] -> return r
                body -> return $ N.urlEncodedBody body r
    >>= \req -> do
        -- 組み立てたHTTPリクエストを発行する
        resp <- N.httpLbs req manager
        -- 受信時間
        tm <- getCurrentTime
        -- ログDBへ
        storeLogDB
            Loghttp { loghttpUrl            = show url
                    , loghttpScheme         = N.uriScheme url
                    , loghttpUserInfo       = Maybe.maybe "" N.uriUserInfo $ N.uriAuthority url
                    , loghttpHost           = Maybe.maybe "" N.uriRegName $ N.uriAuthority url
                    , loghttpPort           = Maybe.maybe "" N.uriPort $ N.uriAuthority url
                    , loghttpPath           = N.uriPath url
                    , loghttpQuery          = N.uriQuery url
                    , loghttpFragment       = N.uriFragment url
                    , loghttpReqMethod      = B8.unpack $ N.method req
                    , loghttpReqHeader      = show header
                    , loghttpReqCookie      = show cookie
                    , loghttpRespDatetime   = tm
                    , loghttpRespStatus     = show $ N.responseStatus resp
                    , loghttpRespVersion    = show $ N.responseVersion resp
                    , loghttpRespHeader     = show $ N.responseHeaders resp
                    , loghttpRespCookie     = show $ N.destroyCookieJar $ N.responseCookieJar resp
                    , loghttpRespBody       = BL8.toStrict $ N.responseBody resp
                    }
        -- HTTPレスポンスを返却
        return resp

{-
 - HTTP ResponseからUtf8 HTMLを取り出す関数
 -}
takeBodyFromResponse :: N.Response BL8.ByteString -> T.Text
takeBodyFromResponse resp =
    -- HTTPレスポンスヘッダから"Content-Type"を得る
    let contentType =   [ T.pack $ B8.unpack v
                        | ("Content-Type", v) <- N.responseHeaders resp
                        ]
    in
    {-
     - RFC2616による定義
     - "https://www.w3.org/Protocols/rfc2616/rfc2616-sec3.html#sec3.7"
     - media-type     = type "/" subtype *( ";" parameter )
     - type           = token
     - subtype        = token
     -}
    let chunks = List.concat $ map (T.toLower . T.strip) . T.split (== ';') <$> contentType in
    -- "Content-Type"から"charset"を取り出すついでに全て小文字に
    let param = [ (T.toLower k, T.toLower v)
                | k : v : _ <- [T.split (== '=') v | v <- chunks]
                ]
    in
    let charset = T.unpack <$> List.lookup "charset" param in
    -- UTF-8へ
    (\str -> case convToUtf8 charset str of
        Right utf8text -> utf8text
        -- デコードの失敗は無視する
        -- たぶんこれはただのバイナリ
        Left msg -> withTrace msg $ T.pack $ BL8.unpack str
    ) $ N.responseBody resp
    where
    {-
     - 文字コード変換関数
     -}
    convToUtf8 :: Maybe IConv.EncodingName
                    -> BL8.ByteString
                    -> Either Encoding.Error.UnicodeException T.Text
    convToUtf8 Nothing = TE.decodeUtf8'
    convToUtf8 (Just cs) = TE.decodeUtf8' . IConv.convert cs "UTF-8"

{-
 - 絶対リンクへ変換する関数
 -}
toAbsURI :: N.URI -> T.Text -> Maybe N.URI
toAbsURI baseURI href =
    fmap (`N.relativeTo` baseURI) (N.parseURIReference $ T.unpack href)

{-
 - 属性を取り出す関数
 -}
takeAttr :: Eq str => (str -> Bool) -> Maybe (TS.TagTree str) -> Maybe str
takeAttr predicade tag =
    tag
    >>= \(TS.TagBranch _ as _) -> Maybe.listToMaybe [v | (k, v) <- as, predicade k]

{-
 - POSTリクエストボディを組み立てる関数
 -}
mkPostReqBody :: [[TS.Attribute T.Text]] -> [(B8.ByteString, B8.ByteString)]
mkPostReqBody attributes =
    -- "type"属性は無視して(name, value)のタプルを作る
    [
        (B8.pack $ T.unpack $ Maybe.fromMaybe "" $ List.lookup "name" a
        ,B8.pack $ T.unpack $ Maybe.fromMaybe "" $ List.lookup "value" a
        )
    | a <- attributes ]

{-
 - ログインページからログインしてHTTPセッション情報を返す関数
 -}
login :: Conf.Info -> N.URI -> IO (Maybe HTTPSession)
login conf loginPage = do
    -- HTTPリクエストヘッダ
    let reqHeader = [ (N.hAccept, "text/html, text/plain, text/css")
                    , (N.hAcceptCharset, "UTF-8")
                    , (N.hAcceptLanguage, "ja, en;q=0.5")
                    , (N.hUserAgent, BL8.toStrict $ BL8.pack $ Conf.userAgent conf) ]
    -- HTTPS接続ですよ
    manager <- N.newManager N.tlsManagerSettings
    -- ログインページへアクセス
    resp <- fetchPage manager reqHeader Nothing [] loginPage
    -- ログインページのHTMLをTextで受け取る
    let html = takeBodyFromResponse resp
    {-
     - ログインページの解析をする
     -}
    let tagTree = TS.tagTree $ TS.parseTags html
    -- ログインページ唯一のformタグを取り出す
    let formTag = Maybe.listToMaybe [all | all@(TS.TagBranch nm as cs) <- TS.universeTree tagTree, "form"==T.toLower nm]
    -- formタグの属性を取り出す
    let fmName = takeAttr ((== "name") . T.toLower) formTag
    let fmMethod = takeAttr ((== "method") . T.toLower) formTag
    let fmAction = takeAttr ((== "action") . T.toLower) formTag
    -- formタグ内のinputタグの属性を全て取り出す
    let inputTagAttrs = formTag
                        >>= \(TS.TagBranch _ _ cs) ->
                            return [as | TS.TagOpen nm as <- TS.flattenTree cs, "input"==T.toLower nm]
    -- POSTリクエストを組み立てる
    let orgPostReqBody = Maybe.maybe [] mkPostReqBody inputTagAttrs
    let postReqBody =   [ ("clientCD", B8.pack $ Conf.loginID conf)
                        , ("passwd", B8.pack $ Conf.loginPassword conf) ]
                        ++ List.filter (\(k, _) -> (k/="clientCD")&&(k/="passwd")) orgPostReqBody
    -- POSTリクエストを送信するURL
    let postActionURL = toAbsURI loginPage =<< fmAction
    case postActionURL of
        Nothing -> return Nothing
        Just url ->
            -- フォームのaction属性ページへアクセス
            fetchPage manager reqHeader Nothing postReqBody url
            -- 返値組み立て
            >>= \resp -> return $ Just
                HTTPSession { sLoginPageURI = loginPage
                            , sManager      = manager
                            , sReqHeaders   = reqHeader
                            , sRespCookies  = N.responseCookieJar resp
                            , sTopPageHTML  = takeBodyFromResponse resp
                            }

{-
 - fetchPage関数の相対リンク版
 -}
fetchInRelativePath :: M.MonadIO m => HTTPSession -> T.Text -> m (N.Response BL8.ByteString)
fetchInRelativePath session relPath =
    let uri = Maybe.fromJust $ toAbsURI (sLoginPageURI session) relPath in
    let cookies = case N.destroyCookieJar $ sRespCookies session of
                    []-> Nothing
                    _ -> Just $ sRespCookies session
    in
    fetchPage (sManager session) (sReqHeaders session) cookies [] uri

{-
 - linkTextをクリックする関数
 -}
type SessionReaderMonadT m = M.ReaderT HTTPSession m [T.Text]
clickLinkText :: M.MonadIO m => T.Text -> [T.Text] -> SessionReaderMonadT m
clickLinkText linkText htmls = do
    --let linkPaths = Maybe.catMaybes $ map (lookup linkText . getPageCaptionAndLink) htmls
    let linkPaths = Maybe.mapMaybe (lookup linkText . getPageCaptionAndLink) htmls
    M.mapM fetch linkPaths
    where
    fetch path = do
        session <- M.ask
        resp <- fetchInRelativePath session path
        return $ takeBodyFromResponse resp

{-
 - GM / LMページからリンクテキストとリンクのタプルを取り出す関数
 -}
getPageCaptionAndLink :: T.Text -> [(T.Text, T.Text)]
getPageCaptionAndLink html =
    let utree = TS.universeTree $ TS.tagTree $ TS.parseTags html in
    [(as,cs) | TS.TagBranch n as cs <- utree, "a"==T.toLower n]
    >>= \(as,cs) -> List.zip
        -- リンクテキストを取り出す
        [t | TS.TagText t <- TS.flattenTree cs]
        -- リンクを取り出す
        [v | (k, v) <- as, "href"==T.toLower k]

{-
 - targetのフレームを処理する
 -}
frameDispacher :: M.MonadIO m
                    => [T.Text] -> (T.Text, [T.Text] -> SessionReaderMonadT m)
                    -> SessionReaderMonadT m
frameDispacher htmls (targetFrmName, action) =
    case htmls of
        [] -> return []
        h : hs -> do
            -- frameタグから属性の(name, src)タプルを作る
            let frames = [as | TS.TagOpen nm as <- TS.parseTags h, "frame" == T.toLower nm]
                        >>= Maybe.maybeToList . buildNameSrc
            -- targetのフレームを処理する
            case List.lookup targetFrmName frames of
                Nothing -> return []
                Just linkPath -> do
                    session <- M.ask
                    resp <- fetchInRelativePath session linkPath
                    r <- action [takeBodyFromResponse resp]
                    s <- frameDispacher hs (targetFrmName, action)
                    return (r ++ s)
    where
    {-
     - frameタグから属性の(name, src)タプルを作る
     -}
    buildNameSrc :: [TS.Attribute T.Text] -> Maybe (T.Text, T.Text)
    buildNameSrc attrList = do
        name <- Maybe.listToMaybe [v | (k, v) <- attrList, "name"==T.toLower k]
        src  <- Maybe.listToMaybe [v | (k, v) <- attrList, "src"==T.toLower k]
        Just (name, src)

{-
 - ログアウトする関数
 -}
logout :: HTTPSession -> IO ()
logout session = do
    M.runReaderT func session >> return ()
    where
    func =
        let commands =  [ ("GM", clickLinkText "■ログアウト")
                        , ("CT", return)] in
        let seed = sTopPageHTML session in
        M.foldM frameDispacher [seed] commands

{-
 - スクレイピングに失敗した場合の例外送出
 -}
failureAtScraping :: M.MonadIO m => T.Text -> m a
failureAtScraping =
    M.liftIO . throwIO . UnexpectedHTMLException . T.unpack

{-
 - トップページからcommandsの指示通りに
 - フレームにアクセスしてスクレイピングした結果を返す関数
 -}
obeyCommand :: (M.MonadIO m, Scraper.Contents co)
            => ([T.Text] -> Either T.Text co)
            -> [(T.Text, [T.Text] -> SessionReaderMonadT m)]
            -> M.ReaderT HTTPSession m co
obeyCommand scraper commands =
    M.ask
    >>= \sess -> M.foldM frameDispacher [sTopPageHTML sess] commands
    >>= Either.either (failureAtScraping) (return) . scraper

{-
 - "お知らせ"を得る
 - "ホーム" -> "お知らせ" のページからスクレイピングする関数
 -}
fetchFraHomeAnnounce :: M.ReaderT HTTPSession IO Scraper.FraHomeAnnounce
fetchFraHomeAnnounce =
    let commands =  [("GM", clickLinkText "ホーム")
                    ,("LM", clickLinkText "お知らせ")
                    ,("CT", return)]
    in
    obeyCommand Scraper.scrapingFraHomeAnnounce commands

{-
 - 現在の保有株情報を得る
 - "株式取引" -> "現物売" のページからスクレイピングする関数
 -}
fetchFraStkSell :: M.ReaderT HTTPSession IO Scraper.FraStkSell
fetchFraStkSell =
    let commands =  [("GM", clickLinkText "株式取引")
                    ,("LM", clickLinkText "現物売")
                    ,("CT", return)]
    in
    obeyCommand Scraper.scrapingFraStkSell commands

{-
 - 現在の資産情報を得る
 - "資産状況" -> "余力情報" のページからスクレイピングする関数
 -}
fetchFraAstSpare :: M.ReaderT HTTPSession IO Scraper.FraAstSpare
fetchFraAstSpare =
    let commands =  [("GM", clickLinkText "資産状況")
                    ,("LM", clickLinkText "余力情報")
                    ,("CT", return)]
    in
    obeyCommand Scraper.scrapingFraAstSpare commands

