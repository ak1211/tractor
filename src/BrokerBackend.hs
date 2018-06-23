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
Module      :  BrokerBackend
Description :  broker backend
Copyright   :  (c) 2016 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  POSIX

証券会社とやりとりするモジュールです。

>>> :set -XOverloadedStrings
-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeFamilies          #-}
module BrokerBackend
    ( HTTPSession(..)
    , DontHaveStocksToSellException (..)
    , fetchHTTP
    , takeBodyFromResponse
    , toAbsoluteURI
    , fetchHTTPInRelativePath
    , mkCustomPostReq
    , waitMS
    , fetchPageWithSession
    ) where
import qualified Codec.Text.IConv                 as IConv
import           Control.Applicative              ((<|>))
import qualified Control.Concurrent               as CC
import           Control.Exception.Safe
import qualified Control.Monad                    as M
import qualified Control.Monad.Logger             as ML
import qualified Control.Monad.Reader             as M
import qualified Control.Monad.Trans.Resource     as MR
import qualified Data.Attoparsec.ByteString.Char8 as Ap
import qualified Data.ByteString.Char8            as B8
import qualified Data.ByteString.Lazy.Char8       as BL8
import           Data.Char                        (Char)
import qualified Data.Either                      as Either
import qualified Data.List                        as List
import qualified Data.Maybe                       as Maybe
import           Data.Monoid                      (mempty, (<>))
import           Data.Text.Encoding.Error         (UnicodeException)
import qualified Data.Text.Lazy                   as TL
import qualified Data.Text.Lazy.Builder           as TLB
import qualified Data.Text.Lazy.Encoding          as TLE
import qualified Data.Time                        as Time
import qualified Data.Typeable
import qualified Database.Persist                 as DB
import qualified Database.Persist.MySQL           as MySQL
import qualified Database.Persist.Sql             as DB
import qualified Network.HTTP.Conduit             as N
import qualified Network.HTTP.Types.Header        as Header
import           Network.URI                      (URI)
import qualified Network.URI                      as URI

import qualified Conf
import qualified GenScraper                       as GS
import           Model

-- |
-- HTTP / HTTPSセッション情報
data HTTPSession = HTTPSession
    { sLoginPageURI :: URI
    , sManager      :: N.Manager
    , sReqHeaders   :: Header.RequestHeaders
    , sRespCookies  :: N.CookieJar
    , sTopPageHTML  :: TL.Text
    }

instance Show HTTPSession where
    show sess =
        TL.unpack . TLB.toLazyText $ mempty
        <> TLB.fromString (show $ sLoginPageURI sess) <> ", "
        <> TLB.fromString (show $ sReqHeaders sess) <> ", "
        <> TLB.fromString (show $ sRespCookies sess) <> ", "
        <> TLB.fromLazyText (sTopPageHTML sess)

-- |
-- 実行時例外 : 未保有株の売却指示
newtype DontHaveStocksToSellException
    = DontHaveStocksToSellException TL.Text
    deriving (Data.Typeable.Typeable, Eq)

instance Show DontHaveStocksToSellException where
    show (DontHaveStocksToSellException msg) = TL.unpack msg

instance Exception DontHaveStocksToSellException

-- |
-- ログをデーターベースへ格納する
storeAccessLog :: M.MonadIO m => AccessLog -> m ()
storeAccessLog logMessage = M.liftIO $
    ML.runNoLoggingT . MR.runResourceT . MySQL.withMySQLConn Conf.loggingConnInfo . MySQL.runSqlConn $ do
        DB.runMigration migrateAccessLog
        M.void $ DB.insert logMessage

-- |
-- urlに接続して結果を得る
-- アクセスログはDBに記録する
fetchHTTP   :: M.MonadIO m
            => N.Manager
            -> Header.RequestHeaders
            -> Maybe N.CookieJar
            -> [(B8.ByteString, B8.ByteString)]
            -> URI
            -> m (N.Response BL8.ByteString)
fetchHTTP manager header cookieJar postReq url =
    M.liftIO $ do
        -- 指定のHTTPヘッダでHTTPリクエストを作る
        let custom = customRequest postReq . customHeader cookieJar
        req <- custom <$> N.parseRequest (show url)
        -- 組み立てたHTTPリクエストを発行する
        responce <- N.httpLbs req manager
        -- 受信時間
        now <- Time.getCurrentTime
        -- ログDBへ
        storeAccessLog $ packLoghttp now req responce
        -- HTTPレスポンスを返却
        return responce
    where
    -- |
    -- HTTPヘッダ
    customHeader :: Maybe N.CookieJar -> N.Request -> N.Request
    --
    customHeader Nothing req =
        req { N.requestHeaders = header }
    --
    customHeader cj req =
        req { N.cookieJar = cj
            , N.requestHeaders = header }
    -- |
    customRequest :: [(B8.ByteString, B8.ByteString)] -> N.Request -> N.Request
    -- HTTP GET リクエスト
    customRequest []       = id
    -- HTTP POST リクエスト
    customRequest postData = N.urlEncodedBody postData
    --
    --
    packLoghttp :: Time.UTCTime
                -> N.Request
                -> N.Response BL8.ByteString
                -> AccessLog
    packLoghttp receivedAt customReq resp = AccessLog
        { accessLogReceivedAt   = receivedAt
        , accessLogUrl          = show url
        , accessLogScheme       = URI.uriScheme url
        , accessLogUserInfo     = maybe "" URI.uriUserInfo $ URI.uriAuthority url
        , accessLogHost         = maybe "" URI.uriRegName $ URI.uriAuthority url
        , accessLogPort         = maybe "" URI.uriPort $ URI.uriAuthority url
        , accessLogPath         = URI.uriPath url
        , accessLogQuery        = URI.uriQuery url
        , accessLogFragment     = URI.uriFragment url
        , accessLogReqCookie    = show cookieJar
        , accessLogReqHeader    = show $ N.requestHeaders customReq
        , accessLogRespStatus   = show $ N.responseStatus resp
        , accessLogRespVersion  = show $ N.responseVersion resp
        , accessLogRespHeader   = show $ N.responseHeaders resp
        , accessLogRespCookie   = show $ N.responseCookieJar resp
        , accessLogRespBody     = BL8.toStrict $ N.responseBody resp
        }

--
--
type HtmlCharset = B8.ByteString

-- |
-- HTMLから文字コードを取り出す関数
-- 先頭より1024バイトまでで
-- <meta http-equiv="Content-Type" content="text/html; charset=shift_jis"> とか
-- <meta HTTP-EQUIV="Content-type" CONTENT="text/html; charset=x-sjis"> とか
-- <meta charset="shift_jis">とかを探す
--
-- >>> takeCharsetFromHTML "<head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\"></head>"
-- Right "UTF-8"
-- >>> takeCharsetFromHTML "<head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=shift_jis\"></head>"
-- Right "shift_jis"
-- >>> takeCharsetFromHTML "<html><head><meta charset=\"utf-8\"></head></html>"
-- Right "utf-8"
-- >>> takeCharsetFromHTML "<html><head><meta charset=\"shift_jis\"></head></html>"
-- Right "shift_jis"
-- >>> takeCharsetFromHTML "<html><head><meta HTTP-EQUIV=\"Content-type\" CONTENT=\"text/html; charset=x-sjis\"></head></html>"
-- Right "x-sjis"
-- >>> takeCharsetFromHTML "<HTML><HEAD><META HTTP-EQUIV=\"CONTENT-TYPE\" CONTENT=\"TEXT/HTML; CHARSET=X-SJIS\"></HEAD></HTML>"
-- Right "X-SJIS"
--
takeCharsetFromHTML :: BL8.ByteString -> Either String HtmlCharset
takeCharsetFromHTML =
    Ap.parseOnly tag . first1024Bytes
    where
    -- |
    -- 先頭より1024バイト
    first1024Bytes :: BL8.ByteString -> B8.ByteString
    first1024Bytes = BL8.toStrict . BL8.take 1024
    -- |
    -- タグをパースする関数
    tag :: Ap.Parser HtmlCharset
    tag =
        Ap.skipSpace *> Ap.char '<' *> (html401meta <|> html5meta)
        <|>
        next *> tag
        Ap.<?> "tag"
    -- |
    -- 次のタグまで読み飛ばす関数
    next = Ap.skipWhile (/= '>') *> Ap.char '>'
    -- |
    -- HTML 4.01 の meta タグをパースする関数
    -- https://www.w3.org/TR/html401/struct/global.html#edef-META
    html401meta =
        Ap.stringCI "meta"
        *> Ap.skipSpace
        *> Ap.stringCI "http-equiv" *> equal *> wquote *> Ap.stringCI "Content-Type" *> wquote
        *> Ap.skipSpace
        *> Ap.stringCI "content" *> equal *> wquote *> mediaType <* wquote
    -- |
    -- HTML 5 の meta タグをパースする関数
    -- https://www.w3.org/TR/html5/document-metadata.html#meta
    html5meta =
        Ap.stringCI "meta"
        *> Ap.skipSpace
        *> Ap.stringCI "charset" *> equal *> wquote *> token1 <* wquote

--
-- 例 : "text/html; charset=shift_jis"
-- RFC2616 - 3.7 Media Typesによる定義
-- "https://www.w3.org/Protocols/rfc2616/rfc2616-sec3.html#sec3.7"
-- media-type     = type "/" subtype *( ";" parameter )
-- type           = token
-- subtype        = token
mediaType :: Ap.Parser HtmlCharset
mediaType =
    token1 *> Ap.char '/' *> token1
    *> Ap.char ';' *> Ap.skipSpace
    *> Ap.stringCI "charset" *> equal *> token1
    Ap.<?> "media-type"

wquote :: Ap.Parser Char
wquote = Ap.char '\"'
equal :: Ap.Parser Char
equal = Ap.char '='
token1 :: Ap.Parser B8.ByteString
token1 = Ap.takeWhile1 (Ap.inClass "0-9a-zA-Z_-")

-- |
-- HTTPヘッダからcharsetを得る
--
-- >>> takeCharsetFromHTTPHeader [("Content-Length","5962"),("Content-Type","text/html; charset=Shift_JIS")]
-- Right "Shift_JIS"
-- >>> takeCharsetFromHTTPHeader [("Content-Length","5962"),("Content-Type","text/html; charset=utf-8")]
-- Right "utf-8"
-- >>> takeCharsetFromHTTPHeader [("Content-Encoding","gzip"),("Content-Type","text/html;charset=UTF-8")]
-- Right "UTF-8"
-- >>> takeCharsetFromHTTPHeader [("Content-Length","5962")]
-- Left "Content-Type is none"
--
takeCharsetFromHTTPHeader :: Header.ResponseHeaders -> Either String HtmlCharset
takeCharsetFromHTTPHeader headers = do
    -- HTTPヘッダから"Content-Type"を得る
    ct <- maybe (Left "Content-Type is none") Right $ lookup "Content-Type" headers
    -- "Content-Type"からcontentを得る
    Ap.parseOnly mediaType ct

-- |
-- HTTP ResponseからUtf8 HTMLを取り出す関数
takeBodyFromResponse :: N.Response BL8.ByteString -> TL.Text
takeBodyFromResponse resp =
    -- 失敗したらエラーの内容を返却する
    either (TL.pack . show) id
    . toUtf8Text (takeCharset resp) $ N.responseBody resp
    where
    --
    -- HTTPレスポンスヘッダの指定 -> 本文中の指定の順番で文字コードを得る
    takeCharset :: N.Response BL8.ByteString -> String
    takeCharset rp =
        let hdr = takeCharsetFromHTTPHeader $ N.responseHeaders rp
            bdy = takeCharsetFromHTML $ N.responseBody rp
        in
        B8.unpack $ Either.fromRight "LATIN1" (hdr <|> bdy)
    --
    --
    toUtf8Text :: String -> BL8.ByteString -> Either UnicodeException TL.Text
    toUtf8Text "x-sjis" = TLE.decodeUtf8' . toUtf8BS "shift-jis"
    toUtf8Text charset  = TLE.decodeUtf8' . toUtf8BS charset
    --
    --
    toUtf8BS :: String -> BL8.ByteString -> BL8.ByteString
    toUtf8BS charset =
        IConv.convertFuzzy IConv.Transliterate charset "UTF-8"

-- |
-- 絶対リンクへ変換する関数
toAbsoluteURI :: URI -> String -> Maybe URI
toAbsoluteURI baseURI href =
    fmap (`URI.relativeTo` baseURI) (URI.parseURIReference href)

-- |
-- fetchHTTP関数の相対リンク版
fetchHTTPInRelativePath :: HTTPSession -> String -> IO (N.Response BL8.ByteString)
fetchHTTPInRelativePath HTTPSession{..} relativePath = do
    uri <- case toAbsoluteURI sLoginPageURI relativePath of
        Nothing -> throwString "fetchHTTPInRelativePath: link url is not valid"
        Just x  -> return x
    fetchHTTP sManager sReqHeaders (Just sRespCookies) [] uri

-- |
-- サーバーに提出するPost情報を組み立てる
--
-- >>> :{
-- >>> mkCustomPostReq
-- >>>      [(Just "JS_FLG",Just "0"), (Just "ACT_login",Nothing)]
-- >>>      [("JS_FLG","1"), ("user_id","myname"), ("password","mypass")]
-- >>> :}
-- [("JS_FLG","1"),("user_id","myname"),("password","mypass")]
mkCustomPostReq :: [GS.PairNV B8.ByteString]
                -> [(B8.ByteString, B8.ByteString)]
                -> [(B8.ByteString, B8.ByteString)]
mkCustomPostReq original custom =
    -- name が重複した場合はcustomを優先出力し
    -- originalからNothingが混ざっている物を削除するので
    -- 自ずと"ACT_login"は消える仕組み
    List.unionBy (\(a,_) (b,_) -> a == b) custom
        $ Maybe.mapMaybe (\(a,b) -> (,) <$> a <*> b) original

-- |
-- ミリ秒待ち
waitMS :: Int -> IO ()
waitMS milliSec =
    CC.threadDelay (milliSec * 1000)

-- |
-- 引数のページへアクセス
fetchPageWithSession :: M.MonadIO m => HTTPSession -> URI -> m TL.Text
fetchPageWithSession HTTPSession{..} uri =
    takeBodyFromResponse
    <$>
    fetchHTTP sManager sReqHeaders (Just sRespCookies) [] uri



