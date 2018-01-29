{-# LANGUAGE OverloadedStrings #-}
module MatsuiCoJp.BrokerSpec (spec) where
import qualified Control.Monad.Reader         as M
import qualified Control.Monad.Trans.Resource as M
import qualified Data.ByteString.Char8        as B8
import           Data.Conduit                 (($$), (=$))
import qualified Data.Conduit                 as C
import           Data.Maybe                   (fromJust)
import qualified Data.Text.Lazy               as TL
import qualified Data.Text.Lazy.IO            as TL
import           Test.Hspec

import qualified Conf
import qualified GenBroker
import qualified MatsuiCoJp.Broker            as B
import qualified MatsuiCoJp.Scraper           as Scr
import qualified SinkSlack                    as Slack

--
--
successSellOrder :: B8.ByteString -> B.SellOrder
successSellOrder passwd =
    B.SellOrder
        { B.sellOrderPassword   = passwd
        , B.sellOrderCode       = 9399
        , B.sellOrderNominal    = 1
        , B.sellOrderPrice      = 189.0
        }

--
--
failSellOrder :: B8.ByteString -> B.SellOrder
failSellOrder passwd =
    B.SellOrder
        { B.sellOrderPassword   = passwd
        , B.sellOrderCode       = 8888
        , B.sellOrderNominal    = 1
        , B.sellOrderPrice      = 100.0
        }

--
--
fetchUpdatedPriceAndStore :: Conf.InfoBroker -> Conf.Info -> IO ()
fetchUpdatedPriceAndStore broker conf =
    M.runResourceT . GenBroker.siteConn broker ua $ \sess ->
        GenBroker.fetchUpdatedPriceAndStore broker connInfo sess
    where
    connInfo = Conf.connInfoDB $ Conf.mariaDB conf
    ua = Conf.userAgent conf

--
--
sellStock :: Conf.InfoBroker -> Conf.Info -> (B8.ByteString -> B.SellOrder) -> IO ()
sellStock broker conf order =
    M.runResourceT . GenBroker.siteConn broker ua $ \sess -> do
        r <- B.sellStock sess order'
        M.liftIO $ TL.putStrLn (Scr.contents r)
    where
    (Conf.MatsuiCoJp matsuicojp) = broker
    (Conf.InfoMatsuiCoJp account) = matsuicojp
    order' = order $ Conf.dealingsPassword account
    ua = Conf.userAgent conf

--
--
noticeOfCurrentAssets :: Conf.InfoBroker -> Conf.Info -> IO ()
noticeOfCurrentAssets broker@(Conf.MatsuiCoJp matsui) conf =
    GenBroker.noticeOfCurrentAssets broker connInfo $$ sinkReport conf
    where
    connInfo = Conf.connInfoDB $ Conf.mariaDB conf

--
--
noticeOfBrokerageAnnouncement :: Conf.InfoBroker -> Conf.Info -> IO ()
noticeOfBrokerageAnnouncement (Conf.MatsuiCoJp matsui) conf =
    B.noticeOfBrokerageAnnouncement matsui connInfo ua $$ sinkText conf
    where
    connInfo = Conf.connInfoDB $ Conf.mariaDB conf
    ua = Conf.userAgent conf

--
-- ここからHspecテスト
--
spec :: Spec
spec = do
    --
    describe "fetchUpdatedPriceAndStore" $
        it "may successful" $ do
            (Right conf) <- Conf.readJSONFile "conf.json"
            let broker = head $ Conf.brokers conf
            fetchUpdatedPriceAndStore broker conf `shouldReturn` ()
    --
--    describe "sellStock" $ do
--        it "may successful" $
--            confEither <- Conf.readJSONFile "conf.json"
--            let (Right conf) = confEither
--            sellStockOrder conf successSellOrder `shouldReturn` ()
--        it "fail contdition" $
--            (Right conf) <- Conf.readJSONFile "conf.json"
--            let broker = head $ Conf.brokers conf
--            sellStock broker conf failSellOrder `shouldThrow` anyException
    --
    describe "noticeOfCurrentAssets" $
        it "may successful" $ do
            (Right conf) <- Conf.readJSONFile "conf.json"
            let broker = head $ Conf.brokers conf
            noticeOfCurrentAssets broker conf `shouldReturn` ()
    --
    describe "noticeOfBrokerageAnnouncement" $
        it "may successful" $ do
            (Right conf) <- Conf.readJSONFile "conf.json"
            let broker = head $ Conf.brokers conf
            noticeOfBrokerageAnnouncement broker conf `shouldReturn` ()
--
-- ここまでHspecテスト
--

-- レポートをsinkSlackで送信する形式に変換する関数
reportMsg :: M.MonadIO m => Conf.Info -> C.Conduit Slack.Report m Slack.WebHook
reportMsg = Slack.reportMsg . Conf.slack

-- 組み立てられたメッセージをConf.Infoで指定されたSlackへ送る関数
sinkSlack :: Conf.Info -> C.Sink Slack.WebHook IO ()
sinkSlack = Slack.sink . Conf.slack

-- テキストメッセージをsinkSlackで送信する形式に変換する関数
simpleTextMsg :: M.MonadIO m => Conf.Info -> C.Conduit TL.Text m Slack.WebHook
simpleTextMsg = Slack.simpleTextMsg . Conf.slack

--
--
sinkReport conf = reportMsg conf =$ sinkSlack conf
sinkText conf = simpleTextMsg conf =$ sinkSlack conf


