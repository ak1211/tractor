{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module BrokerSpec (spec) where

import qualified Control.Monad.Reader         as M
import qualified Control.Monad.Trans.Resource as M
import           Data.Conduit                 (($$), ($=))
import qualified Data.Conduit                 as C
import           Data.Maybe                   (fromJust)
import qualified Data.Text.Lazy.IO            as TL
import           Test.Hspec

import qualified Conf
import           GenBroker                    as Broker
import           MatsuiCoJp.Broker
import qualified MatsuiCoJp.Scraper           as Scr
import qualified SinkSlack                    as Slack

-- |
-- レポートをsinkSlackで送信する形式に変換する関数
reportMsg :: M.MonadIO m => Conf.Info -> C.Conduit Slack.Report m Slack.WebHook
reportMsg = Slack.reportMsg . Conf.slack

-- |
-- 組み立てられたメッセージをConf.Infoで指定されたSlackへ送る関数
sinkSlack :: Conf.Info -> C.Sink Slack.WebHook IO ()
sinkSlack = Slack.sink . Conf.slack

successSellOrder :: String -> SellOrder
successSellOrder pass = SellOrder
    { sellOrderPassword  = pass
    , sellOrderCode      = 9399
    , sellOrderNominal   = 1
    , sellOrderPrice     = 189.0
    }

failSellOrder :: String -> SellOrder
failSellOrder pass = SellOrder
    { sellOrderPassword  = pass
    , sellOrderCode      = 8888
    , sellOrderNominal   = 1
    , sellOrderPrice     = 100.0
    }

fetchAndStore :: Conf.Info -> IO ()
fetchAndStore conf = do
    let mconf = fromJust (Conf.matsuiCoJp conf)
    M.runResourceT . Broker.siteConn mconf $ \sess ->
        Broker.fetchUpdatedPriceAndStore
        (Conf.connInfoDB $ Conf.mariaDB conf) mconf sess

notice :: Conf.Info -> IO ()
notice conf = do
    let mconf = fromJust (Conf.matsuiCoJp conf)
    Broker.noticeOfCurrentAssets
        (Conf.connInfoDB $ Conf.mariaDB conf) mconf
        $= reportMsg conf $$ sinkSlack conf

sellStockOrder :: Conf.Info -> (String -> SellOrder) -> IO ()
sellStockOrder conf order = do
    let mconf = fromJust (Conf.matsuiCoJp conf)
    M.runResourceT . Broker.siteConn mconf $ \sess -> do
        r <- sellStock sess (order $ Conf.dealingsPassword mconf)
        M.liftIO $ TL.putStrLn (Scr.contents r)

--
-- Hspecテスト
spec :: Spec
spec = do
    describe "fetchAndStore" $ do
        it "may successful" $ do
            confEither <- Conf.readJSONFile "conf.json"
            let (Right conf) = confEither
            fetchAndStore conf `shouldReturn` ()
    describe "sell order" $ do
        it "may successful" $ do
            confEither <- Conf.readJSONFile "conf.json"
            let (Right conf) = confEither
            sellStockOrder conf successSellOrder `shouldReturn` ()
        it "fail contdition" $ do
            confEither <- Conf.readJSONFile "conf.json"
            let (Right conf) = confEither
            sellStockOrder conf failSellOrder `shouldThrow` anyException
    describe "notice" $ do
        it "may successful" $ do
            confEither <- Conf.readJSONFile "conf.json"
            let (Right conf) = confEither
            notice conf `shouldReturn` ()


