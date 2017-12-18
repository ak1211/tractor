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
Copyright   :  (c) 2016, 2017 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  POSIX

証券会社とやりとりするモジュールです。
-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module BrokerBackend
    ( HTTPSession (..)
    , UnexpectedHTMLException (..)
    , DontHaveStocksToSellException (..)
    , fetchPage
    , takeBodyFromResponse
    , toAbsoluteURI
    , fetchInRelativePath
    , failureAtScraping
    ) where

import qualified Codec.Text.IConv             as IConv
import           Control.Exception            (Exception, throwIO)
import qualified Control.Monad.Logger         as ML
import qualified Control.Monad.Reader         as M
import qualified Control.Monad.Trans.Resource as MR
import qualified Data.ByteString.Char8        as B8
import qualified Data.ByteString.Lazy.Char8   as BL8
import qualified Data.List                    as List
import qualified Data.Maybe                   as Maybe
import qualified Data.Text.Lazy               as TL
import qualified Data.Text.Lazy.Encoding      as TE
import qualified Data.Time                    as Tm
import qualified Data.Typeable
import qualified Database.Persist             as DB
import qualified Database.Persist.MySQL       as MySQL
import qualified Database.Persist.Sql         as DB
import qualified Network.HTTP.Conduit         as N
import qualified Network.HTTP.Types.Header    as N
import qualified Network.URI                  as N
import           Text.Parsec                  ((<|>))
import qualified Text.Parsec                  as P
import qualified Text.Parsec.ByteString.Lazy  as P

import qualified Conf
import           Model

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
        show (sLoginPageURI sess)
        ++ ", " ++ show (sReqHeaders sess)
        ++ ", " ++ show (sRespCookies sess)
        ++ ", " ++ TL.unpack (sTopPageHTML sess)

-- |
-- 実行時例外 : 予想外のHTML
newtype UnexpectedHTMLException
    = UnexpectedHTMLException String
    deriving (Data.Typeable.Typeable, Eq)

instance Show UnexpectedHTMLException where
    show (UnexpectedHTMLException msg) = msg

instance Exception UnexpectedHTMLException

-- |
-- 実行時例外 : 未保有株の売却指示
newtype DontHaveStocksToSellException
    = DontHaveStocksToSellException String
    deriving (Data.Typeable.Typeable, Eq)

instance Show DontHaveStocksToSellException where
    show (DontHaveStocksToSellException msg) = msg

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
fetchPage manager header cookie reqBody url =
    M.liftIO $ do
        -- 指定のHTTPヘッダでHTTPリクエストを作る
        req <- customRequest . customHeader <$> N.parseRequest (show url)
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
    customHeader req =
        req { N.cookieJar = cookie, N.requestHeaders = header }
    -- |
    -- HTTPリクエスト
    customRequest req =
        case reqBody of
            []   -> req
            body -> N.urlEncodedBody body req
    --
    --
    packLoghttp :: Tm.UTCTime
                -> N.Request
                -> (N.Response BL8.ByteString)
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
        , accessLogReqCookie    = show cookie
        , accessLogReqHeader    = show $ N.requestHeaders customReq
        , accessLogRespStatus   = show $ N.responseStatus resp
        , accessLogRespVersion  = show $ N.responseVersion resp
        , accessLogRespHeader   = show $ N.responseHeaders resp
        , accessLogRespCookie   = show . N.destroyCookieJar $ N.responseCookieJar resp
        , accessLogRespBody     = BL8.toStrict $ N.responseBody resp
        }

--
--
type HtmlCharset = String

-- |
-- HTTP ResponseからUtf8 HTMLを取り出す関数
takeBodyFromResponse :: N.Response BL8.ByteString -> TL.Text
takeBodyFromResponse resp =
    let bodyHtml = N.responseBody resp in
    -- 先頭より1024バイトまでで
    -- <meta http-equiv="Content-Type" content="text/html; charset=shift_jis"> とか
    -- <meta charset="shift_jis">とかが
    -- 無いか探してみる
    let htmlHead1024B = BL8.take 1024 bodyHtml in
    let htmlCS = case P.parse html "(html header)" htmlHead1024B of
                    Left   _ -> Nothing
                    Right "" -> Nothing
                    Right  r -> Just r
    in
    -- HTTPレスポンスヘッダからエンコードを得る
    let respCS = takeCharset $ N.responseHeaders resp in
    -- HTTPレスポンスヘッダの指定 -> 本文中の指定の順番でエンコードを得る
    case Maybe.listToMaybe $ Maybe.catMaybes [respCS, htmlCS] of
        -- エンコード指定がないので文字化けで返す
        Nothing -> forcedConvUtf8 bodyHtml
        -- UTF-8へ
        Just cs ->
            let u = TE.decodeUtf8' $ IConv.convert cs "UTF-8" bodyHtml in
            -- デコードの失敗はおそらくバイナリなので文字化けで返す
            either (const $ forcedConvUtf8 bodyHtml) id u
    where
    -- |
    -- 文字化けでも無理やり返す関数
    forcedConvUtf8 :: BL8.ByteString -> TL.Text
    forcedConvUtf8 = TL.pack . BL8.unpack
    -- |
    -- htmlをパースする関数
    html :: P.Parser HtmlCharset
    html    =   P.try (P.spaces *> tag)
            <|> P.try (next *> html)
            <|> return ""
    -- |
    -- 次のタグまで読み飛ばす関数
    next :: P.Parser ()
    next    =  P.skipMany1 (P.noneOf ">")
            <* P.char '>'
    -- |
    -- タグをパースする関数
    tag :: P.Parser HtmlCharset
    tag     =  P.char '<'
            *> meta
    -- |
    -- metaタグをパースする関数
    meta :: P.Parser HtmlCharset
    meta    =  P.string "meta"
            *> P.spaces
            *> P.try charset <|> P.try metaHttpEquiv
    -- |
    -- meta http-equivタグをパースする関数
    --
    -- 例 : http-equiv="Content-Type" content="text/html; charset=shift_jis"
    -- https://www.w3.org/TR/html5/document-metadata.html#meta
    metaHttpEquiv :: P.Parser HtmlCharset
    metaHttpEquiv = P.string "http-equiv=" *> contentType
    -- |
    -- meta http-equiv Content-Typeをパースする関数
    contentType :: P.Parser HtmlCharset
    contentType = do
        M.void $ P.string "\"Content-Type\""
        P.spaces
        M.void $ P.string "content="
        M.void $ P.many (P.char '\"')
        c <- content
        M.void $ P.many (P.char '\"')
        return c
    --
    -- 例 : "text/html; charset=shift_jis"
    -- RFC2616 - 3.7 Media Typesによる定義
    -- "https://www.w3.org/Protocols/rfc2616/rfc2616-sec3.html#sec3.7"
    -- media-type     = type "/" subtype *( ";" parameter )
    -- type           = token
    -- subtype        = token
    content :: P.Parser HtmlCharset
    content = do
        M.void $ P.many P.alphaNum
        M.void $ P.char '/'
        M.void $ P.many P.alphaNum
        parameter <|> return ""
    --
    --
    parameter :: P.Parser HtmlCharset
    parameter = do
        M.void $ P.char ';'
        P.spaces
        charset <|> P.many P.alphaNum
    --
    --
    charset :: P.Parser HtmlCharset
    charset = do
        --
        -- 例 : charset="utf-8"
        -- https://www.w3.org/TR/html5/document-metadata.html#meta
        M.void $ P.string "charset="
        M.void $ P.many (P.char '\"')
        cs <- P.many (P.alphaNum <|> P.char '_' <|> P.char '-')
        M.void $ P.many (P.char '\"')
        return cs
    -- |
    -- HTTPレスポンスヘッダからcharsetを得る
    takeCharset :: N.ResponseHeaders -> Maybe String
    takeCharset headers = do
        -- HTTPレスポンスヘッダから"Content-Type"を得る
        ct <- List.find (\kv -> "Content-Type"==fst kv) headers
        -- "Content-Type"からcontentを得る
        case P.parse content "(resp header)". BL8.fromStrict . snd $ ct of
            Left   _ -> Nothing
            Right "" -> Nothing
            Right  r -> Just r

-- |
-- 絶対リンクへ変換する関数
toAbsoluteURI :: N.URI -> TL.Text -> Maybe N.URI
toAbsoluteURI baseURI href =
    fmap (`N.relativeTo` baseURI) (N.parseURIReference $ TL.unpack href)

-- |
-- fetchPage関数の相対リンク版
fetchInRelativePath :: HTTPSession -> TL.Text -> IO (N.Response BL8.ByteString)
fetchInRelativePath session relativePath = do
    uri <- case toAbsoluteURI (sLoginPageURI session) relativePath of
        Nothing -> throwIO $ userError "link url is not valid"
        Just x  -> return x
    let cookies = case N.destroyCookieJar $ sRespCookies session of
                    []-> Nothing
                    _  -> Just (sRespCookies session)
    fetchPage (sManager session) (sReqHeaders session) cookies [] uri

-- |
-- スクレイピングに失敗した場合の例外送出
failureAtScraping :: M.MonadIO m => String -> m a
failureAtScraping =
    M.liftIO . throwIO . UnexpectedHTMLException

