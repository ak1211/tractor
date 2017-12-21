{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module MatsuiCoJp.BrokerSpec (spec) where

import qualified Control.Monad.Reader         as M
import qualified Control.Monad.Trans.Resource as M
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
successSellOrder :: String -> B.SellOrder
successSellOrder passwd =
    B.SellOrder
        { B.sellOrderPassword   = passwd
        , B.sellOrderCode       = 9399
        , B.sellOrderNominal    = 1
        , B.sellOrderPrice      = 189.0
        }

--
--
failSellOrder :: String -> B.SellOrder
failSellOrder passwd =
    B.SellOrder
        { B.sellOrderPassword   = passwd
        , B.sellOrderCode       = 8888
        , B.sellOrderNominal    = 1
        , B.sellOrderPrice      = 100.0
        }

--
--
fetchUpdatedPriceAndStore :: Conf.Info -> IO ()
fetchUpdatedPriceAndStore conf =
    M.runResourceT . GenBroker.siteConn mconf $ \sess ->
        GenBroker.fetchUpdatedPriceAndStore connInfo mconf sess
    where
    mconf = fromJust $ Conf.matsuiCoJp conf
    connInfo = Conf.connInfoDB $ Conf.mariaDB conf

--
--
sellStock :: Conf.Info -> (String -> B.SellOrder) -> IO ()
sellStock conf order =
    M.runResourceT . GenBroker.siteConn mconf $ \sess -> do
        r <- B.sellStock sess order'
        M.liftIO $ TL.putStrLn (Scr.contents r)
    where
    mconf = fromJust $ Conf.matsuiCoJp conf
    order' = order $ Conf.dealingsPassword mconf

--
--
noticeOfCurrentAssets :: Conf.Info -> IO ()
noticeOfCurrentAssets conf =
    GenBroker.noticeOfCurrentAssets connInfo mconf $$ sinkReport conf
    where
    mconf = fromJust $ Conf.matsuiCoJp conf
    connInfo = Conf.connInfoDB $ Conf.mariaDB conf

--
--
noticeOfBrokerageAnnouncement :: Conf.Info -> IO ()
noticeOfBrokerageAnnouncement conf =
    B.noticeOfBrokerageAnnouncement connInfo mconf $$ sinkText conf
    where
    mconf = fromJust $ Conf.matsuiCoJp conf
    connInfo = Conf.connInfoDB $ Conf.mariaDB conf

--
-- ここからHspecテスト
--
spec :: Spec
spec = do
    --
    describe "fetchUpdatedPriceAndStore" $ do
        it "may successful" $ do
            confEither <- Conf.readJSONFile "conf.json"
            let (Right conf) = confEither
            fetchUpdatedPriceAndStore conf `shouldReturn` ()
    --
    describe "sellStock" $ do
--        it "may successful" $ do
--            confEither <- Conf.readJSONFile "conf.json"
--            let (Right conf) = confEither
--            sellStockOrder conf successSellOrder `shouldReturn` ()
        it "fail contdition" $ do
            confEither <- Conf.readJSONFile "conf.json"
            let (Right conf) = confEither
            sellStock conf failSellOrder `shouldThrow` anyException
    --
    describe "noticeOfCurrentAssets" $ do
        it "may successful" $ do
            confEither <- Conf.readJSONFile "conf.json"
            let (Right conf) = confEither
            noticeOfCurrentAssets conf `shouldReturn` ()
    --
    describe "noticeOfBrokerageAnnouncement" $ do
        it "may successful" $ do
            confEither <- Conf.readJSONFile "conf.json"
            let (Right conf) = confEither
            noticeOfBrokerageAnnouncement conf `shouldReturn` ()
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


