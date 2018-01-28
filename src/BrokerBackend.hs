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
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
module BrokerBackend
    ( HTTPSession(..)
    , UnexpectedHTMLException(..)
    , DontHaveStocksToSellException (..)
    , fetchPage
    , takeBodyFromResponse
    , toAbsoluteURI
    , fetchInRelativePath
    , mkCustomPostReq
    , failureAtScraping
    ) where
import qualified Codec.Text.IConv             as IConv
import qualified Control.Applicative          as App
import           Control.Exception            (Exception, throwIO)
import qualified Control.Monad                as M
import qualified Control.Monad.Logger         as ML
import qualified Control.Monad.Reader         as M
import qualified Control.Monad.Trans.Resource as MR
import qualified Data.ByteString.Char8        as B8
import qualified Data.ByteString.Lazy.Char8   as BL8
import qualified Data.List                    as List
import qualified Data.Maybe                   as Maybe
import           Data.Monoid                  (mempty, (<>))
import qualified Data.Text.Lazy               as TL
import qualified Data.Text.Lazy.Builder       as TLB
import qualified Data.Text.Lazy.Encoding      as TLE
import qualified Data.Time                    as Tm
import qualified Data.Typeable
import qualified Database.Persist             as DB
import qualified Database.Persist.MySQL       as MySQL
import qualified Database.Persist.Sql         as DB
import qualified Network.HTTP.Conduit         as N
import qualified Network.HTTP.Types.Header    as N
import qualified Network.URI                  as N
import           Text.Parsec                  ((<?>), (<|>))
import qualified Text.Parsec                  as P
import qualified Text.Parsec.ByteString.Lazy  as P

import qualified Conf
import           Model
import qualified ScraperBackend               as SB

-- |
-- 実行時例外 : 予想外のHTML
newtype UnexpectedHTMLException
    = UnexpectedHTMLException TL.Text
    deriving (Data.Typeable.Typeable, Eq)

instance Show UnexpectedHTMLException where
    show (UnexpectedHTMLException msg) = TL.unpack msg

instance Exception UnexpectedHTMLException

-- |
-- HTTP / HTTPSセッション情報
data HTTPSession = HTTPSession
    { sLoginPageURI :: N.URI
    , sManager      :: N.Manager
    , sReqHeaders   :: N.RequestHeaders
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
fetchPage   :: M.MonadIO m
            => N.Manager
            -> N.RequestHeaders
            -> Maybe N.CookieJar
            -> [(B8.ByteString, B8.ByteString)]
            -> N.URI
            -> m (N.Response BL8.ByteString)
fetchPage manager header cookieJar postReq url =
    M.liftIO $ do
        -- 指定のHTTPヘッダでHTTPリクエストを作る
        let custom = (customRequest postReq) . (customHeader cookieJar)
        req <- custom <$> N.parseRequest (show url)
        -- 組み立てたHTTPリクエストを発行する
        responce <- N.httpLbs req manager
        -- 受信時間
        now <- Tm.getCurrentTime
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
    packLoghttp :: Tm.UTCTime
                -> N.Request
                -> N.Response BL8.ByteString
                -> AccessLog
    packLoghttp receivedAt customReq resp = AccessLog
        { accessLogReceivedAt   = receivedAt
        , accessLogUrl          = show url
        , accessLogScheme       = N.uriScheme url
        , accessLogUserInfo     = maybe "" N.uriUserInfo $ N.uriAuthority url
        , accessLogHost         = maybe "" N.uriRegName $ N.uriAuthority url
        , accessLogPort         = maybe "" N.uriPort $ N.uriAuthority url
        , accessLogPath         = N.uriPath url
        , accessLogQuery        = N.uriQuery url
        , accessLogFragment     = N.uriFragment url
        , accessLogReqCookie    = show $ cookieJar
        , accessLogReqHeader    = show $ N.requestHeaders customReq
        , accessLogRespStatus   = show $ N.responseStatus resp
        , accessLogRespVersion  = show $ N.responseVersion resp
        , accessLogRespHeader   = show $ N.responseHeaders resp
        , accessLogRespCookie   = show $ N.responseCookieJar resp
        , accessLogRespBody     = BL8.toStrict $ N.responseBody resp
        }

--
--
type HtmlCharset = String

-- |
--
-- HTMLから文字コードを取り出す関数
-- 先頭より1024バイトまでで
-- <meta http-equiv="Content-Type" content="text/html; charset=shift_jis"> とか
-- <meta charset="shift_jis">とかを探す
--
-- >>> charsetFromHTML "<head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\"></head>"
-- Right "UTF-8"
-- >>> charsetFromHTML "<head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=shift_jis\"></head>"
-- Right "shift_jis"
-- >>> charsetFromHTML "<html><head><meta charset=\"utf-8\"></head></html>"
-- Right "utf-8"
-- >>> charsetFromHTML "<html><head><meta charset=\"shift_jis\"></head></html>"
-- Right "shift_jis"
charsetFromHTML :: BL8.ByteString -> Either P.ParseError HtmlCharset
charsetFromHTML =
    P.parse html "(parse html header)"
    . first1024Bytes
    where
    -- |
    -- 先頭より1024バイト
    first1024Bytes :: BL8.ByteString -> BL8.ByteString
    first1024Bytes = BL8.take 1024
    -- |
    -- htmlをパースする関数
    html :: P.Parser HtmlCharset
    html =
        P.try (P.spaces *> tag)
        <|> P.try (next *> html)
        <?> "end of html"
    -- |
    -- 次のタグまで読み飛ばす関数
    next :: P.Parser ()
    next = P.skipMany1 (P.noneOf ">") <* P.char '>'
    -- |
    -- タグをパースする関数
    tag :: P.Parser HtmlCharset
    tag = P.char '<' *> meta
    -- |
    -- ダブルクオート
    wquote = P.many (P.char '\"')
    -- |
    -- metaタグをパースする関数
    meta :: P.Parser HtmlCharset
    meta =
        P.string "meta" *> P.spaces
        *> (P.try charset5 <|> P.try httpEquiv)
        where
        -- |
        -- meta http-equivタグをパースする関数
        --
        -- 例 : http-equiv="Content-Type" content="text/html; charset=shift_jis"
        -- https://www.w3.org/TR/html5/document-metadata.html#meta
        httpEquiv :: P.Parser HtmlCharset
        httpEquiv =
            P.string "http-equiv=" *> contentType
        -- |
        -- meta http-equiv Content-Typeをパースする関数
        contentType :: P.Parser HtmlCharset
        contentType =
            P.string "\"Content-Type\"" *> P.spaces
            *> P.string "content=" *> wquote *> content <* wquote
    --
    -- 例 : charset="utf-8"
    -- https://www.w3.org/TR/html5/document-metadata.html#meta
    charset5 :: P.Parser HtmlCharset
    charset5 =
        P.string "charset=" *> wquote *> description <* wquote
        where
        description = P.many (P.alphaNum <|> P.char '_' <|> P.char '-')

--
-- 例 : "text/html; charset=shift_jis"
-- RFC2616 - 3.7 Media Typesによる定義
-- "https://www.w3.org/Protocols/rfc2616/rfc2616-sec3.html#sec3.7"
-- media-type     = type "/" subtype *( ";" parameter )
-- type           = token
-- subtype        = token
content :: P.Parser HtmlCharset
content =
    typesubtype *> P.spaces *> P.char ';' *> parameter
    <?> "broken content"
    where
    typesubtype = P.many (P.alphaNum <|> P.char '/')
    --
    --
    parameter :: P.Parser HtmlCharset
    parameter =
        P.spaces *> charset <?> "missing charset"
    --
    --
    charset :: P.Parser HtmlCharset
    charset =
        P.string "charset="
        *> P.many (P.alphaNum <|> P.char '_' <|> P.char '-')

-- |
-- HTTPヘッダからcharsetを得る
--
-- >>> charsetFromHTTPHeader [("Content-Length","5962"),("Content-Type","text/html; charset=Shift_JIS")]
-- Just "Shift_JIS"
-- >>> charsetFromHTTPHeader [("Content-Length","5962"),("Content-Type","text/html; charset=utf-8")]
-- Just "utf-8"
-- >>> charsetFromHTTPHeader [("Content-Encoding","gzip"),("Content-Type","text/html;charset=UTF-8")]
-- Just "UTF-8"
-- >>> charsetFromHTTPHeader [("Content-Length","5962")]
-- Nothing
charsetFromHTTPHeader :: N.ResponseHeaders -> Maybe HtmlCharset
charsetFromHTTPHeader headers =
    -- HTTPヘッダから"Content-Type"を得る
    BL8.fromStrict <$> lookup "Content-Type" headers
    -- "Content-Type"からcontentを得る
    >>= return . P.parse content "(http header)"
    >>= \case
        Left   _ -> Nothing
        Right "" -> Nothing
        Right  r -> Just r

-- |
-- HTTP ResponseからUtf8 HTMLを取り出す関数
takeBodyFromResponse :: N.Response BL8.ByteString -> TL.Text
takeBodyFromResponse resp =
    let
        html = N.responseBody resp
        -- HTMLから文字コードを得る
        csetHtml = case charsetFromHTML html of
                    Left   _ -> Nothing
                    Right "" -> Nothing
                    Right  r -> Just r
        -- HTTPレスポンスヘッダから文字コードを得る
        csetResp = charsetFromHTTPHeader $ N.responseHeaders resp
        -- HTTPレスポンスヘッダの指定 -> 本文中の指定の順番で文字コードを得る
        charset = csetHtml App.<|> csetResp
        -- UTF-8テキスト
        utf8txt = (flip toUtf8) html =<< charset
    in
    -- デコードの失敗はあきらめて文字化けで返却
    maybe (forcedConv html) id utf8txt
    where
    --
    --
    toUtf8  :: IConv.EncodingName
            -> BL8.ByteString
            -> Maybe TL.Text
    toUtf8 charset =
        either
        (const Nothing)
        Just
        . TLE.decodeUtf8'
        . IConv.convert charset "UTF-8"
    -- |
    -- 文字化けでも無理やり返す関数
    forcedConv :: BL8.ByteString -> TL.Text
    forcedConv = TL.pack . BL8.unpack

-- |
-- 絶対リンクへ変換する関数
toAbsoluteURI :: N.URI -> String -> Maybe N.URI
toAbsoluteURI baseURI href =
    fmap (`N.relativeTo` baseURI) (N.parseURIReference href)

-- |
-- fetchPage関数の相対リンク版
fetchInRelativePath :: HTTPSession -> String -> IO (N.Response BL8.ByteString)
fetchInRelativePath HTTPSession{..} relativePath = do
    uri <- case toAbsoluteURI sLoginPageURI relativePath of
        Nothing -> throwIO $ userError "link url is not valid"
        Just x  -> return x
    fetchPage sManager sReqHeaders (Just sRespCookies) [] uri

-- |
-- サーバーに提出するPost情報を組み立てる
--
-- >>> :{
-- >>> mkCustomPostReq
-- >>>      [(Just "JS_FLG",Just "0"), (Just "ACT_login",Nothing)]
-- >>>      [("JS_FLG","1"), ("user_id","myname"), ("password","mypass")]
-- >>> :}
-- [("JS_FLG","1"),("user_id","myname"),("password","mypass")]
mkCustomPostReq :: [SB.PairNV B8.ByteString]
                -> [(B8.ByteString, B8.ByteString)]
                -> [(B8.ByteString, B8.ByteString)]
mkCustomPostReq original custom =
    -- name が重複した場合はcustomを優先出力し
    -- originalからNothingが混ざっている物を削除するので
    -- 自ずと"ACT_login"は消える仕組み
    List.unionBy (\(a,_) (b,_) -> a == b) custom
        $ Maybe.mapMaybe (\(a,b) -> (,) <$> a <*> b) original

-- |
-- スクレイピングに失敗した場合の例外送出
failureAtScraping :: M.MonadIO m => TL.Text -> m a
failureAtScraping =
    M.liftIO . throwIO . UnexpectedHTMLException

