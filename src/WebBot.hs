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
    , SellOrderSet (..)
    , sellStock
    ) where

import qualified Scraper
import qualified DataBase

import Debug.Trace (trace)
import Prelude hiding (catch)
import Control.Exception hiding (throw)
import Safe
import qualified Conf

import Control.Applicative ((<$>), (<*>), (<*))
import Text.Parsec ((<|>))
import qualified Text.Parsec as P
import qualified Text.Parsec.ByteString.Lazy as P

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
import qualified Data.Char as Char
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

import qualified Control.Concurrent as CC

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
fetchPage   :: M.MonadIO m
            => N.Manager
            -> N.RequestHeaders
            -> Maybe N.CookieJar
            -> [(B8.ByteString, B8.ByteString)]
            -> N.URI
            -> m (N.Response BL8.ByteString)
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

type HtmlCharset = String
{-
 - HTTP ResponseからUtf8 HTMLを取り出す関数
 -}
takeBodyFromResponse :: N.Response BL8.ByteString -> T.Text
takeBodyFromResponse resp =
    let bodyHtml = N.responseBody resp in
    -- 先頭より1024バイトまでで
    -- <meta http-equiv="Content-Type" content="text/html; charset=shift_jis"> とか
    -- <meta charset="shift_jis">とかが
    -- 無いか探してみる
    let htmlHead1024B = BL8.take 1024 bodyHtml in
    let htmlCS = case (P.parse html "(html header)" htmlHead1024B) of
                    Left   _ -> Nothing
                    Right "" -> Nothing
                    Right  r -> Just r
    in
    -- HTTPレスポンスヘッダからエンコードを得る
    let respCS = takeCharset $ N.responseHeaders resp in
    -- 本文中の指定 -> HTTPレスポンスヘッダの指定の順番でエンコードを得る
    case Maybe.listToMaybe $ Maybe.catMaybes [htmlCS, respCS] of
        -- エンコード指定がないので文字化けで返す
        Nothing -> forcedConvUtf8 bodyHtml
        -- UTF-8へ
        Just cs ->
            let u = TE.decodeUtf8' $ IConv.convert cs "UTF-8" bodyHtml in
            -- デコードの失敗はおそらくバイナリなので文字化けで返す
            Either.either (\_ -> forcedConvUtf8 bodyHtml) id u
    where
    -- 文字化けでも無理やり返す
    forcedConvUtf8 :: BL8.ByteString -> T.Text
    forcedConvUtf8 = T.pack . BL8.unpack
    -- htmlをパースする関数
    html :: P.Parser HtmlCharset
    html =
        P.try (P.spaces >> tag)
        <|>
        P.try (next >> html)
        <|>
        return ""
    -- 次のタグまで読み飛ばす関数
    next :: P.Parser ()
    next =
        P.skipMany1 (P.noneOf ">") <* P.char '>'
    -- タグをパースする関数
    tag :: P.Parser HtmlCharset
    tag =
        P.char '<' >> meta
    -- metaタグをパースする関数
    meta :: P.Parser HtmlCharset
    meta = do
        P.string "meta"
        P.spaces
        P.try (metaCharset) <|> P.try (metaHttpEquiv)
    -- meta http-equivタグをパースする関数
    metaHttpEquiv :: P.Parser HtmlCharset
    metaHttpEquiv = do
        {-
            例 : http-equiv="Content-Type" content="text/html; charset=shift_jis"
            https://www.w3.org/TR/html5/document-metadata.html#meta
        -}
        P.string "http-equiv="
        contentType
    {-
    -}
    contentType :: P.Parser HtmlCharset
    contentType = do
        P.string "\"Content-Type\""
        P.spaces
        P.string "content="
        P.many (P.char '\"')
        c <- content
        P.many (P.char '\"')
        return c
    {-
        例 : "text/html; charset=shift_jis"
        RFC2616 - 3.7 Media Typesによる定義
        "https://www.w3.org/Protocols/rfc2616/rfc2616-sec3.html#sec3.7"
        media-type     = type "/" subtype *( ";" parameter )
        type           = token
        subtype        = token
    -}
    content :: P.Parser HtmlCharset
    content = do
        _ <- P.many P.alphaNum
        P.char '/'
        _ <- P.many P.alphaNum
        parameter <|> return ""
    --
    parameter :: P.Parser HtmlCharset
    parameter = do
        P.char ';'
        P.spaces
        charset <|> P.many P.alphaNum
    --
    charset :: P.Parser HtmlCharset
    charset = do
        P.string "charset="
        P.many (P.char '\"')
        cs <- P.many (P.alphaNum <|> P.char '_' <|> P.char '-')
        P.many (P.char '\"')
        return cs
    -- meta charsetタグをパースする関数
    metaCharset :: P.Parser HtmlCharset
    metaCharset = do
        {-
            例 : charset="utf-8"
            https://www.w3.org/TR/html5/document-metadata.html#meta
        -}
        P.string "charset="
        P.many (P.char '\"')
        cs <- P.many (P.alphaNum <|> P.char '_' <|> P.char '-')
        P.many (P.char '\"')
        return cs
    -- HTTPレスポンスヘッダからcharsetを得る
    takeCharset :: N.ResponseHeaders -> Maybe String
    takeCharset headers =
        -- HTTPレスポンスヘッダから"Content-Type"を得る
        List.find (\kv -> "Content-Type"==fst kv) headers
        -- "Content-Type"からcontentを得る
        >>= Just . BL8.fromStrict . snd
        >>= Just . (P.parse content "(resp header)")
        >>= \case
            Left   _ -> Nothing
            Right "" -> Nothing
            Right  r -> Just r

{-
 - 絶対リンクへ変換する関数
 -}
toAbsURI :: N.URI -> T.Text -> Maybe N.URI
toAbsURI baseURI href =
    fmap (`N.relativeTo` baseURI) (N.parseURIReference $ T.unpack href)

{-
 - formのactionを実行する関数
 -}
doPostAction :: N.Manager
                -> N.RequestHeaders
                -> Maybe N.CookieJar
                -> [(B8.ByteString, B8.ByteString)]
                -> N.URI
                -> T.Text
                -> IO (Maybe (N.Response BL8.ByteString))
doPostAction manager reqHeader cookie customPostReq pageURI html = do
    let uTree = TS.universeTree $ TS.tagTree $ TS.parseTags html
    let formTags = [all | all@(TS.TagBranch nm _ _) <- uTree, "form"==T.toLower nm]
    case formTags of
        -- ページ唯一のformタグを取り出す
        x : _ -> action x
        _ -> return Nothing
    where
    action :: TS.TagTree T.Text -> IO (Maybe (N.Response BL8.ByteString))
    action = \case
        form@(TS.TagBranch _  attrs childNodes) -> do
            let defaultPostReqBody = defaultRequest form
            -- formタグの属性を取り出す
            let fmAction = Maybe.listToMaybe $ [v | (k,v)<-attrs, "action"==T.toLower k]
            let postReqBody = Maybe.catMaybes
                                $ (\(ks,vs) -> List.zipWith chooseDefaultOrCustomReq ks vs)
                                $ List.unzip defaultPostReqBody
            -- POSTリクエストを送信するURL
            let postActionURL = toAbsURI pageURI =<< fmAction
            -- フォームのaction属性ページへアクセス

    --        T.putStrLn . T.pack . show $ selectTagAttrs
            T.putStrLn . T.pack . show $ postActionURL
            T.putStrLn . T.pack . show $ postReqBody


            M.mapM (fetchPage manager reqHeader cookie postReqBody) postActionURL
        _ -> return Nothing
    --
    chooseDefaultOrCustomReq :: B8.ByteString -> B8.ByteString -> Maybe (B8.ByteString, B8.ByteString)
    chooseDefaultOrCustomReq k v =
        case List.lookup k customPostReq of
            Just cv -> Just (k, cv)
            -- x yは指定の物以外削除
            Nothing | B8.isSuffixOf ".x" k -> Nothing
            Nothing | B8.isSuffixOf ".y" k -> Nothing
            _ ->  Just (k, v)

    --
    takeValue :: [TS.Attribute T.Text] -> T.Text -> Maybe B8.ByteString
    takeValue attrs name =
        B8.pack . T.unpack . snd <$> List.find ((==) (T.toCaseFold name) . T.toCaseFold . fst) attrs
    -- 未入力時のフォームリクエストを得る
    defaultRequest :: TS.TagTree T.Text -> [(B8.ByteString, B8.ByteString)]
    defaultRequest (TS.TagBranch _  attrs childNodes) =
        let sel = [selectTag all | all@(TS.TagBranch nm _ _) <- TS.universeTree childNodes, "select"==T.toLower nm] in
        let ias = [as | TS.TagOpen nm as <- TS.flattenTree childNodes, "input"==T.toLower nm] in
        let inp = List.concat $ map inputTag ias in
        let img = List.concat $ map inputTypeImage ias in
        Maybe.catMaybes sel ++ inp ++ img
        where
        --
        selectTag :: TS.TagTree T.Text -> Maybe (B8.ByteString, B8.ByteString)
        selectTag (TS.TagBranch _ attrs children) =
            -- タグ内のoptionタグを全て取り出す
            let options = [as | TS.TagOpen nm as <- TS.flattenTree children, "option"==T.toLower nm] in
            List.find (\as -> Maybe.isJust $ takeValue as "selected") options
            >>= \as -> (\k v -> (k,v)) <$> takeValue attrs "name" <*> takeValue as "value"
        --
        inputTag :: [TS.Attribute T.Text] -> [(B8.ByteString, B8.ByteString)]
        inputTag attrs =
            let f = takeValue attrs in
            -- (name, value)のタプルを作る
            case f "type" of
                Just "checkbox" | Maybe.isNothing (f "checked") -> []
                Just "radio" | Maybe.isNothing (f "checked") -> []
                _ -> Maybe.maybeToList $ (\a b -> (a,b)) <$> f "name" <*> f "value"
        --
        -- input type="image"
        inputTypeImage :: [TS.Attribute T.Text] -> [(B8.ByteString, B8.ByteString)]
        inputTypeImage attrs =
            let f = takeValue attrs in
            let name = f "name" in
            -- (name, value)のタプルを作る
            case f "type" of
                Just "image" | Maybe.isJust name ->
                    let nm = Maybe.fromJust name in
                    [(nm `B8.append` ".x", "0"), (nm `B8.append` ".y", "0")]
                _ -> []

{-
 - HTTPセッション中にformのactionを実行する関数
 -}
doPostActionOnSession :: HTTPSession
                -> [(B8.ByteString, B8.ByteString)]
                -> T.Text
                -> IO (Maybe (N.Response BL8.ByteString))
doPostActionOnSession sess customPostReq html =
    doPostAction
        (sManager sess)
        (sReqHeaders sess)
        (Just $ sRespCookies sess)
        customPostReq
        (sLoginPageURI sess)
        html

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
    let customPostReq = [ ("clientCD", B8.pack $ Conf.loginID conf)
                        , ("passwd", B8.pack $ Conf.loginPassword conf)
                        ]
    resp <- doPostAction manager reqHeader Nothing customPostReq loginPage html
    return $ mkSession manager reqHeader <$> resp
    where
    -- 返値組み立て
    mkSession manager reqHeader resp =
        HTTPSession { sLoginPageURI = loginPage
                    , sManager      = manager
                    , sReqHeaders   = reqHeader
                    , sRespCookies  = N.responseCookieJar resp
                    , sTopPageHTML  = takeBodyFromResponse resp
                    }


{-
 - fetchPage関数の相対リンク版
 -}
fetchInRelativePath :: M.MonadIO m => T.Text -> M.ReaderT HTTPSession m (N.Response BL8.ByteString)
fetchInRelativePath relPath = do
    session <- M.ask
    let uri = Maybe.fromJust $ toAbsURI (sLoginPageURI session) relPath
    let cookies = case N.destroyCookieJar $ sRespCookies session of
                    []-> Nothing
                    _ -> Just $ sRespCookies session
    fetchPage (sManager session) (sReqHeaders session) cookies [] uri

{-
 - linkTextをクリックする関数
 -}
type SessionReaderMonadT m = M.ReaderT HTTPSession m [T.Text]
clickLinkText :: M.MonadIO m => T.Text -> [T.Text] -> SessionReaderMonadT m
clickLinkText linkText htmls = do
    let linkPaths = Maybe.mapMaybe (lookup linkText . getPageCaptionAndLink) htmls
    M.mapM fetch linkPaths
    where
    fetch path = do
        session <- M.ask
        resp <- fetchInRelativePath path
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
                    html <- takeBodyFromResponse <$> fetchInRelativePath linkPath
                    r <- action [html]
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
 - "お知らせ"を得る
 - "ホーム" -> "お知らせ" のページからスクレイピングする関数
 -}
fetchFraHomeAnnounce :: M.ReaderT HTTPSession IO Scraper.FraHomeAnnounce
fetchFraHomeAnnounce =
    let fM s = M.foldM frameDispacher [sTopPageHTML s]
                [("GM", clickLinkText "ホーム")
                ,("LM", clickLinkText "お知らせ")
                ,("CT", return)]
    in
    Either.either (failureAtScraping) (pure)
    <$> Scraper.scrapingFraHomeAnnounce =<< fM =<< M.ask

{-
 - 現在の保有株情報を得る
 - "株式取引" -> "現物売" のページからスクレイピングする関数
 -}
fetchFraStkSell :: M.ReaderT HTTPSession IO Scraper.FraStkSell
fetchFraStkSell =
    let fM s = M.foldM frameDispacher [sTopPageHTML s]
                [("GM", clickLinkText "株式取引")
                ,("LM", clickLinkText "現物売")
                ,("CT", return)]
    in
    Either.either (failureAtScraping) (pure)
    <$> Scraper.scrapingFraStkSell =<< fM =<< M.ask

{-
 - 現在の資産情報を得る
 - "資産状況" -> "余力情報" のページからスクレイピングする関数
 -}
fetchFraAstSpare :: M.ReaderT HTTPSession IO Scraper.FraAstSpare
fetchFraAstSpare =
    let fM s = M.foldM frameDispacher [sTopPageHTML s]
                [("GM", clickLinkText "資産状況")
                ,("LM", clickLinkText "余力情報")
                ,("CT", return)]
    in
    Either.either (failureAtScraping) (pure)
    <$> Scraper.scrapingFraAstSpare =<< fM =<< M.ask

{-
 - 売り注文を出す関数
 -}
{-
 - 注文情報
 -}
data SellOrderSet = SellOrderSet {
    osPassword  :: String,
    osCode      :: Int,
    osNominal   :: Int,
    osPrice     :: Double
}

sellStock :: SellOrderSet -> M.ReaderT HTTPSession IO Scraper.OrderConfirmed
sellStock order =
    let fM s = M.foldM frameDispacher [sTopPageHTML s]
                [("GM", clickLinkText "株式取引")
                ,("LM", clickLinkText "現物売")
                ,("CT", doSellOrder order)]
    in
    Either.either (failureAtScraping) (pure)
    <$> Scraper.scrapingOrderConfirmed =<< fM =<< M.ask
    where
    --
    doSellOrder :: M.MonadIO m => SellOrderSet -> [T.Text] -> SessionReaderMonadT m
    doSellOrder os htmls = do
        a <- clickSellOrderLink order htmls
        M.liftIO $ CC.threadDelay (300 * 1000)
        b <- submitSellOrderPage order a
        M.liftIO $ CC.threadDelay (300 * 1000)
        submitConfirmPage order b
    --
    clickSellOrderLink :: M.MonadIO m => SellOrderSet -> [T.Text] -> SessionReaderMonadT m
    clickSellOrderLink os htmls = do
        fss <- Either.either (failureAtScraping) (pure) $ Scraper.scrapingFraStkSell htmls

        -- 所有株の中からcodeで指定された銘柄の売り注文ページリンクを取り出す
        let eqCode = ((==) (osCode os)) . Scraper.hsCode
        sellOrderUri <- case List.find eqCode $ Scraper.fsStocks fss of
            Nothing ->
                M.liftIO . throwIO . DontHaveStocksToSellException .
                Printf.printf "証券コード%dの株式を所有してないので売れません" $ osCode os
            Just stock -> pure (Scraper.hsSellOrderUrl stock)
        -- 売り注文ページを読み込む
        sellOrderPage <- case sellOrderUri of
            Nothing ->
                M.liftIO . throwIO . UnexpectedHTMLException .
                Printf.printf "証券コード%dの株式注文ページに行けません" $ osCode os
            Just uri -> takeBodyFromResponse <$> fetchInRelativePath uri
        return [sellOrderPage]
    --
    submitSellOrderPage :: M.MonadIO m => SellOrderSet -> [T.Text] -> SessionReaderMonadT m
    submitSellOrderPage os htmls = do
        html <- case htmls of
            []  -> failureAtScraping "株式注文ページを受け取れていません。"
            -- 株式注文ページには次のページが無いので先頭のみを取り出す
            x:_ -> pure x
        {-
         - 売り注文ページのPOSTリクエストを組み立てる
         - 以下は初期値のまま
         - name="orderNari" 成行チェックボックス
         - name="execCondCD" 執行条件ラジオボタン
         - name="validDt" 有効期間ラジオボタン
         -}
        let customPostReq = [("orderNominal", B8.pack . show $ osNominal os)    -- 株数
                            ,("orderPrc", B8.pack . show $ osPrice os)          -- 値段
                            ,("tyukakuButton.x", "57")                          -- 注文確認ボタンのクリック位置
                            ,("tyukakuButton.y", "10")                          -- 注文確認ボタンのクリック位置
                            ]
        -- 売り注文ページのフォームを提出する
        session <- M.ask
        M.liftIO $ doPostActionOnSession session customPostReq html
        >>= \case
            Nothing -> M.liftIO . throwIO . UnexpectedHTMLException $
                        Printf.printf "証券コード%dの注文確認ページに行けません" (osCode os)
            Just r -> return [takeBodyFromResponse r]
    --
    submitConfirmPage :: M.MonadIO m => SellOrderSet -> [T.Text] -> SessionReaderMonadT m
    submitConfirmPage os htmls = do
        html <- case htmls of
            []  -> failureAtScraping "注文確認ページを受け取れていません。"
            -- 注文確認ページには次のページが無いので先頭のみを取り出す
            x:_ -> pure x
        {-
         - 注文確認ページのPOSTリクエストを組み立てる
         -}
        let customPostReq = [("pinNo", B8.pack $ osPassword os)]    -- 取引暗証番号
        -- 注文確認ページのフォームを提出する
        session <- M.ask
        M.liftIO $ doPostActionOnSession session customPostReq html
        >>= \case
            Nothing -> M.liftIO . throwIO . UnexpectedHTMLException $
                        Printf.printf "証券コード%dの注文終了ページに行けません" (osCode os)
            Just r -> return [takeBodyFromResponse r]

