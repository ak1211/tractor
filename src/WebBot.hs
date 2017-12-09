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
Module      :  WebBot
Description :  Crawl websites
Copyright   :  (c) 2016, 2017 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  POSIX

ウェブサイトを巡回するモジュールです。
-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module WebBot
    ( UnexpectedHTMLException (..)
    , DontHaveStocksToSellException (..)
    , HTTPSession (..)
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

import qualified Codec.Text.IConv            as IConv
import qualified Control.Concurrent          as CC
import           Control.Exception
import qualified Control.Monad               as M
import qualified Control.Monad.IO.Class      as M
import qualified Data.ByteString.Char8       as B8
import qualified Data.ByteString.Lazy.Char8  as BL8
import qualified Data.List                   as List
import qualified Data.Maybe                  as Maybe
import qualified Data.Text.Lazy              as T
import qualified Data.Text.Lazy.Encoding     as TE
import qualified Data.Time                   as Tm
import qualified Data.Typeable
import qualified Database.Persist            as DB
import qualified Database.Persist.Sqlite     as Sqlite
import qualified Network.HTTP.Conduit        as N
import qualified Network.HTTP.Types.Header   as N
import qualified Network.URI                 as N
import qualified Text.HTML.TagSoup           as TS
import qualified Text.HTML.TagSoup.Tree      as TS
import           Text.Parsec                 ((<|>))
import qualified Text.Parsec                 as P
import qualified Text.Parsec.ByteString.Lazy as P
import qualified Text.Printf                 as Printf

import qualified Conf
import           Model
import qualified Scraper

{- |
    実行時例外 : 予想外のHTML
-}
newtype UnexpectedHTMLException
    = UnexpectedHTMLException String
    deriving (Data.Typeable.Typeable, Eq)

instance Show UnexpectedHTMLException where
    show (UnexpectedHTMLException s) = s

instance Exception UnexpectedHTMLException

{- |
    実行時例外 : 未保有株の売却指示
-}
newtype DontHaveStocksToSellException
    = DontHaveStocksToSellException String
    deriving (Data.Typeable.Typeable, Eq, Show)

instance Exception DontHaveStocksToSellException

{- |
    HTTP / HTTPSセッション情報
-}
data HTTPSession = HTTPSession {
    sLoginPageURI :: N.URI,
    sManager      :: N.Manager,
    sReqHeaders   :: N.RequestHeaders,
    sRespCookies  :: N.CookieJar,
    sTopPageHTML  :: T.Text
}

instance Show HTTPSession where
    show (HTTPSession a _ c d e) =
        Printf.printf "%s, %s, %s, %s" (show a) (show c) (show d) (show e)

{- |
    ログをデーターベースへ格納する
-}
storeLogDB :: M.MonadIO m => Loghttp -> m ()
storeLogDB logMessage = M.liftIO $
    Sqlite.runSqlite "loghttp.sqlite3" $ do
        Sqlite.runMigration migrateLogTable
        M.void $ DB.insert logMessage

{- |
    urlに接続して結果を得るついでにDBに記録する
-}
fetchPage   :: M.MonadIO m
            => N.Manager
            -> N.RequestHeaders
            -> Maybe N.CookieJar
            -> [(B8.ByteString, B8.ByteString)]
            -> N.URI
            -> m (N.Response BL8.ByteString)
fetchPage manager header cookie reqBody url = M.liftIO $ do
    -- HTTPリクエストを作る
    -- HTTPヘッダを指定する
    req <- customHeader <$> N.parseRequest (show url)
    -- HTTPリクエストボディを指定する
    let customReq = case reqBody of
                []   -> req
                body -> N.urlEncodedBody body req
    -- 組み立てたHTTPリクエストを発行する
    resp <- N.httpLbs customReq manager
    -- 受信時間
    tm <- Tm.getCurrentTime
    -- ログDBへ
    storeLogDB
        Loghttp { loghttpUrl            = show url
                , loghttpScheme         = N.uriScheme url
                , loghttpUserInfo       = maybe "" N.uriUserInfo $ N.uriAuthority url
                , loghttpHost           = maybe "" N.uriRegName $ N.uriAuthority url
                , loghttpPort           = maybe "" N.uriPort $ N.uriAuthority url
                , loghttpPath           = N.uriPath url
                , loghttpQuery          = N.uriQuery url
                , loghttpFragment       = N.uriFragment url
                , loghttpReqCookie      = show cookie
                , loghttpReqBody        = show customReq
                , loghttpRespDatetime   = tm
                , loghttpRespStatus     = show $ N.responseStatus resp
                , loghttpRespVersion    = show $ N.responseVersion resp
                , loghttpRespHeader     = show $ N.responseHeaders resp
                , loghttpRespCookie     = show $ N.destroyCookieJar $ N.responseCookieJar resp
                , loghttpRespBody       = BL8.toStrict $ N.responseBody resp
                }
    -- HTTPレスポンスを返却
    return resp
    where
    -- HTTPヘッダを指定する
    customHeader req =
        req { N.cookieJar = cookie
            , N.requestHeaders = header
            }

type HtmlCharset = String
{- |
    HTTP ResponseからUtf8 HTMLを取り出す関数
-}
takeBodyFromResponse :: N.Response BL8.ByteString -> T.Text
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
    -- | 文字化けでも無理やり返す関数
    forcedConvUtf8 :: BL8.ByteString -> T.Text
    forcedConvUtf8 = T.pack . BL8.unpack
    -- | htmlをパースする関数
    html :: P.Parser HtmlCharset
    html    =   P.try (P.spaces *> tag)
            <|> P.try (next *> html)
            <|> return ""
    -- | 次のタグまで読み飛ばす関数
    next :: P.Parser ()
    next    =  P.skipMany1 (P.noneOf ">")
            <* P.char '>'
    -- | タグをパースする関数
    tag :: P.Parser HtmlCharset
    tag     =  P.char '<'
            *> meta
    -- | metaタグをパースする関数
    meta :: P.Parser HtmlCharset
    meta    =  P.string "meta"
            *> P.spaces
            *> P.try charset <|> P.try metaHttpEquiv
    -- | meta http-equivタグをパースする関数
    {-
        例 : http-equiv="Content-Type" content="text/html; charset=shift_jis"
        https://www.w3.org/TR/html5/document-metadata.html#meta
    -}
    metaHttpEquiv :: P.Parser HtmlCharset
    metaHttpEquiv = P.string "http-equiv=" *> contentType
    -- | meta http-equiv Content-Typeをパースする関数
    contentType :: P.Parser HtmlCharset
    contentType = do
        M.void $ P.string "\"Content-Type\""
        P.spaces
        M.void $ P.string "content="
        M.void $ P.many (P.char '\"')
        c <- content
        M.void $ P.many (P.char '\"')
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
        M.void $ P.many P.alphaNum
        M.void $ P.char '/'
        M.void $ P.many P.alphaNum
        parameter <|> return ""
    --
    parameter :: P.Parser HtmlCharset
    parameter = do
        M.void $ P.char ';'
        P.spaces
        charset <|> P.many P.alphaNum
    --
    charset :: P.Parser HtmlCharset
    charset = do
        {-
            例 : charset="utf-8"
            https://www.w3.org/TR/html5/document-metadata.html#meta
        -}
        M.void $ P.string "charset="
        M.void $ P.many (P.char '\"')
        cs <- P.many (P.alphaNum <|> P.char '_' <|> P.char '-')
        M.void $ P.many (P.char '\"')
        return cs
    -- | HTTPレスポンスヘッダからcharsetを得る
    takeCharset :: N.ResponseHeaders -> Maybe String
    takeCharset headers = do
        -- HTTPレスポンスヘッダから"Content-Type"を得る
        ct <- List.find (\kv -> "Content-Type"==fst kv) headers
        -- "Content-Type"からcontentを得る
        case P.parse content "(resp header)". BL8.fromStrict . snd $ ct of
            Left   _ -> Nothing
            Right "" -> Nothing
            Right  r -> Just r

{- |
    絶対リンクへ変換する関数
-}
toAbsURI :: N.URI -> T.Text -> Maybe N.URI
toAbsURI baseURI href =
    fmap (`N.relativeTo` baseURI) (N.parseURIReference $ T.unpack href)

{- |
    formのactionを実行する関数
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
    let formTags = [formTag | formTag@(TS.TagBranch nm _ _) <- uTree, "form"==T.toLower nm]
    case formTags of
        -- ページ唯一のformタグを取り出す
        x : _ -> action x
        _     -> return Nothing
    where
    action :: TS.TagTree T.Text -> IO (Maybe (N.Response BL8.ByteString))
    action = \case
        form@(TS.TagBranch _  attrs _) -> do
            let defaultPostReqBody = defaultRequest form
            -- formタグの属性を取り出す
            let fmAction = Maybe.listToMaybe [v | (k,v)<-attrs, "action"==T.toLower k]
            let postReqBody = Maybe.catMaybes
                                $ uncurry (List.zipWith chooseDefaultOrCustomReq)
                                $ List.unzip defaultPostReqBody
            -- POSTリクエストを送信するURL
            let postActionURL = toAbsURI pageURI =<< fmAction
            -- フォームのaction属性ページへアクセス
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
            _       ->  Just (k, v)

    --
    takeValue :: [TS.Attribute T.Text] -> T.Text -> Maybe B8.ByteString
    takeValue attrs name =
        B8.pack . T.unpack . snd <$> List.find ((==) (T.toCaseFold name) . T.toCaseFold . fst) attrs
    -- | 未入力時のフォームリクエストを得る
    defaultRequest :: TS.TagTree T.Text -> [(B8.ByteString, B8.ByteString)]
    defaultRequest (TS.TagLeaf _) = undefined
    defaultRequest (TS.TagBranch _  _ childNodes) =
        let sel = [selectTag br | br@(TS.TagBranch nm _ _) <- TS.universeTree childNodes, "select"==T.toLower nm] in
        let ias = [as | TS.TagOpen nm as <- TS.flattenTree childNodes, "input"==T.toLower nm] in
        let inp = List.concatMap inputTag ias in
        let img = List.concatMap inputTypeImage ias in
        Maybe.catMaybes sel ++ inp ++ img
        where
        --
        selectTag :: TS.TagTree T.Text -> Maybe (B8.ByteString, B8.ByteString)
        selectTag (TS.TagLeaf _) = undefined
        selectTag (TS.TagBranch _ attrs' children) =
            -- タグ内のoptionタグを全て取り出す
            let options = [as | TS.TagOpen nm as <- TS.flattenTree children, "option"==T.toLower nm] in
            List.find (\as -> Maybe.isJust $ takeValue as "selected") options
            >>= \as -> (\k v -> (k,v)) <$> takeValue attrs' "name" <*> takeValue as "value"
        --
        inputTag :: [TS.Attribute T.Text] -> [(B8.ByteString, B8.ByteString)]
        inputTag attrs' =
            let f = takeValue attrs' in
            -- (name, value)のタプルを作る
            case f "type" of
                Just "checkbox" | Maybe.isNothing (f "checked") -> []
                Just "radio" | Maybe.isNothing (f "checked") -> []
                _ -> Maybe.maybeToList $ (\a b -> (a,b)) <$> f "name" <*> f "value"
        --
        -- input type="image"
        inputTypeImage :: [TS.Attribute T.Text] -> [(B8.ByteString, B8.ByteString)]
        inputTypeImage attrs' =
            let f = takeValue attrs' in
            let name = f "name" in
            -- (name, value)のタプルを作る
            case f "type" of
                Just "image" | Maybe.isJust name ->
                    let nm = Maybe.fromJust name in
                    [(nm `B8.append` ".x", "0"), (nm `B8.append` ".y", "0")]
                _ -> []

{- |
    HTTPセッション中にformのactionを実行する関数
-}
doPostActionOnSession :: HTTPSession
                -> [(B8.ByteString, B8.ByteString)]
                -> T.Text
                -> IO (Maybe (N.Response BL8.ByteString))
doPostActionOnSession s customPostReq =
    doPostAction (sManager s) (sReqHeaders s) (Just $ sRespCookies s) customPostReq (sLoginPageURI s)

{- |
    ログインページからログインしてHTTPセッション情報を返す関数
-}
login :: Conf.Info -> N.URI -> IO (Maybe HTTPSession)
login conf loginUri = do
    -- HTTPリクエストヘッダ
    let reqHeader = [ (N.hAccept, "text/html, text/plain, text/css")
                    , (N.hAcceptCharset, "UTF-8")
                    , (N.hAcceptLanguage, "ja, en;q=0.5")
                    , (N.hUserAgent, BL8.toStrict $ BL8.pack $ Conf.userAgent conf) ]
    -- ログインID&パスワード
    let customPostReq = [ ("clientCD", B8.pack $ Conf.loginID conf)
                        , ("passwd", B8.pack $ Conf.loginPassword conf)
                        ]
    -- HTTPS接続ですよ
    manager <- N.newManager N.tlsManagerSettings
    -- ログインページへアクセス
    loginPageBody <- takeBodyFromResponse <$> fetchPage manager reqHeader Nothing [] loginUri
    -- ログインページでID&パスワードをsubmit
    resp <- doPostAction manager reqHeader Nothing customPostReq loginUri loginPageBody
    let session = mkSession manager reqHeader <$> resp
    return session
    where
    -- 返値組み立て
    mkSession manager reqHeader resp =
        HTTPSession { sLoginPageURI = loginUri
                    , sManager      = manager
                    , sReqHeaders   = reqHeader
                    , sRespCookies  = N.responseCookieJar resp
                    , sTopPageHTML  = takeBodyFromResponse resp
                    }

{- |
    fetchPage関数の相対リンク版
-}
fetchInRelativePath :: HTTPSession -> T.Text -> IO (N.Response BL8.ByteString)
fetchInRelativePath session relativePath = do
    uri <- case toAbsURI (sLoginPageURI session) relativePath of
        Nothing -> throwIO $ userError "link url is not valid"
        Just x  -> return x
    let cookies = case N.destroyCookieJar $ sRespCookies session of
                    []-> Nothing
                    _  -> Just (sRespCookies session)
    fetchPage (sManager session) (sReqHeaders session) cookies [] uri

{- |
    linkTextをクリックする関数
-}
clickLinkText :: HTTPSession -> T.Text -> [T.Text] -> IO [T.Text]
clickLinkText session linkText htmls =
    M.mapM fetch linkPaths
    where
    --
    linkPaths = Maybe.mapMaybe (lookup linkText . getPageCaptionAndLink) htmls
    --
    fetch relativePath =
        takeBodyFromResponse <$> fetchInRelativePath session relativePath

{- |
    GM / LMページからリンクテキストとリンクのタプルを取り出す関数
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

{- |
    targetのフレームを処理する
-}
dispatchFrameSet   :: HTTPSession
                    -> [T.Text]
                    -> (T.Text, [T.Text] -> IO [T.Text])
                    -> IO [T.Text]
dispatchFrameSet session htmls (targetFrmName, action) =
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
                    html <- takeBodyFromResponse <$> fetchInRelativePath session linkPath
                    r <- action [html]
                    s <- dispatchFrameSet session hs (targetFrmName, action)
                    return (r ++ s)
    where
    {- |
        frameタグから属性の(name, src)タプルを作る
    -}
    buildNameSrc :: [TS.Attribute T.Text] -> Maybe (T.Text, T.Text)
    buildNameSrc attrList = do
        name <- Maybe.listToMaybe [v | (k, v) <- attrList, "name"==T.toLower k]
        src  <- Maybe.listToMaybe [v | (k, v) <- attrList, "src"==T.toLower k]
        Just (name, src)

{- |
    ログアウトする関数
-}
logout :: HTTPSession -> IO ()
logout session =
    M.void $
        M.foldM
            (dispatchFrameSet session)
            [sTopPageHTML session]
            [ ("GM", clickLinkText session "■ログアウト")
            , ("CT", return)
            ]

{- |
    スクレイピングに失敗した場合の例外送出
-}
failureAtScraping :: M.MonadIO m => T.Text -> m a
failureAtScraping =
    M.liftIO . throwIO . UnexpectedHTMLException . T.unpack

{- |
    "お知らせ"を得る
    "ホーム" -> "お知らせ" のページからスクレイピングする関数
-}
fetchFraHomeAnnounce :: HTTPSession -> IO Scraper.FraHomeAnnounce
fetchFraHomeAnnounce session = do
    contents <- Scraper.scrapingFraHomeAnnounce <$> fetchContents
    case contents of
        Right r  -> return r
        Left err -> failureAtScraping err
    where
    fetchContents =
        M.foldM
            (dispatchFrameSet session)
            [sTopPageHTML session]
            [ ("GM", clickLinkText session "ホーム")
            , ("LM", clickLinkText session "お知らせ")
            , ("CT", return)
            ]

{- |
    現在の保有株情報を得る
    "株式取引" -> "現物売" のページからスクレイピングする関数
-}
fetchFraStkSell :: HTTPSession -> IO Scraper.FraStkSell
fetchFraStkSell session = do
    contents <- Scraper.scrapingFraStkSell <$> fetchContents
    case contents of
        Right r  -> return r
        Left err -> failureAtScraping err
    where
    fetchContents =
        M.foldM
            (dispatchFrameSet session)
            [sTopPageHTML session]
            [ ("GM", clickLinkText session "株式取引")
            , ("LM", clickLinkText session "現物売")
            , ("CT", return)
            ]

{- |
    現在の資産情報を得る
    "資産状況" -> "余力情報" のページからスクレイピングする関数
-}
fetchFraAstSpare :: HTTPSession -> IO Scraper.FraAstSpare
fetchFraAstSpare session = do
    contents <- Scraper.scrapingFraAstSpare <$> fetchContents
    case contents of
        Right r  -> return r
        Left err -> failureAtScraping err
    where
    fetchContents =
        M.foldM
            (dispatchFrameSet session)
            [sTopPageHTML session]
            [ ("GM", clickLinkText session "資産状況")
            , ("LM", clickLinkText session "余力情報")
            , ("CT", return)
            ]

{- |
    注文情報
-}
data SellOrderSet = SellOrderSet
    { osPassword :: String
    , osCode     :: Int
    , osNominal  :: Int
    , osPrice    :: Double
    }

{- |
    売り注文を出す関数
-}
sellStock :: HTTPSession -> SellOrderSet -> IO Scraper.OrderConfirmed
sellStock session order = do
    contents <- Scraper.scrapingOrderConfirmed <$> fetchContents
    case contents of
        Right r  -> return r
        Left err -> failureAtScraping err
    where
    fetchContents =
        M.foldM
            (dispatchFrameSet session)
            [sTopPageHTML session]
            [ ("GM", clickLinkText session "株式取引")
            , ("LM", clickLinkText session "現物売")
            , ("CT", doSellOrder order)
            ]
    --
    doSellOrder :: SellOrderSet -> [T.Text] -> IO [T.Text]
    doSellOrder _ htmls = do
        a <- clickSellOrderLink order htmls
        CC.threadDelay (300 * 1000)
        b <- submitSellOrderPage order a
        CC.threadDelay (300 * 1000)
        submitConfirmPage order b
    --
    clickSellOrderLink :: SellOrderSet -> [T.Text] -> IO [T.Text]
    clickSellOrderLink os htmls = do
        fss <- either failureAtScraping pure $ Scraper.scrapingFraStkSell htmls

        -- 所有株の中からcodeで指定された銘柄の売り注文ページリンクを取り出す
        let eqCode = (==) (osCode os) . Scraper.hsCode
        sellOrderUri <- case List.find eqCode $ Scraper.fsStocks fss of
            Nothing ->
                throwIO . DontHaveStocksToSellException .
                Printf.printf "証券コード%dの株式を所有してないので売れません" $ osCode os
            Just stock -> pure (Scraper.hsSellOrderUrl stock)
        -- 売り注文ページを読み込む
        sellOrderPage <- case sellOrderUri of
            Nothing ->
                throwIO . UnexpectedHTMLException .
                Printf.printf "証券コード%dの株式注文ページに行けません" $ osCode os
            Just uri -> takeBodyFromResponse <$> fetchInRelativePath session uri
        return [sellOrderPage]
    --
    submitSellOrderPage :: SellOrderSet -> [T.Text] -> IO [T.Text]
    submitSellOrderPage os htmls = do
        html <- case htmls of
            []  -> failureAtScraping "株式注文ページを受け取れていません。"
            -- 株式注文ページには次のページが無いので先頭のみを取り出す
            x:_ -> pure x
        {-
            売り注文ページのPOSTリクエストを組み立てる
            以下は初期値のまま
            name="orderNari" 成行チェックボックス
            name="execCondCD" 執行条件ラジオボタン
            name="validDt" 有効期間ラジオボタン
        -}
        let customPostReq = [("orderNominal", B8.pack . show $ osNominal os)    -- 株数
                            ,("orderPrc", B8.pack . show $ osPrice os)          -- 値段
                            ,("tyukakuButton.x", "57")                          -- 注文確認ボタンのクリック位置
                            ,("tyukakuButton.y", "10")                          -- 注文確認ボタンのクリック位置
                            ]
        -- 売り注文ページのフォームを提出する
        doPostActionOnSession session customPostReq html
        >>= \case
            Nothing -> M.liftIO . throwIO . UnexpectedHTMLException $
                        Printf.printf "証券コード%dの注文確認ページに行けません" (osCode os)
            Just r -> return [takeBodyFromResponse r]
    --
    submitConfirmPage :: SellOrderSet -> [T.Text] -> IO [T.Text]
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
        doPostActionOnSession session customPostReq html
        >>= \case
            Nothing -> M.liftIO . throwIO . UnexpectedHTMLException $
                        Printf.printf "証券コード%dの注文終了ページに行けません" (osCode os)
            Just r -> return [takeBodyFromResponse r]

