{-# LANGUAGE OverloadedStrings #-}
module SBIsecCoJp.BrokerSpec (spec) where
import qualified Control.Monad                as M
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
import qualified SBIsecCoJp.Broker            as B
import qualified SBIsecCoJp.Scraper           as S
import qualified SinkSlack                    as Slack

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
noticeOfCurrentAssets :: Conf.InfoBroker -> Conf.Info -> IO ()
noticeOfCurrentAssets broker@(Conf.SBIsecCoJp sbi) conf =
    GenBroker.noticeOfCurrentAssets broker connInfo $$ sinkReport conf
    where
    connInfo = Conf.connInfoDB $ Conf.mariaDB conf

--
--
noticeOfBrokerageAnnouncement :: Conf.InfoBroker -> Conf.Info -> IO ()
noticeOfBrokerageAnnouncement (Conf.SBIsecCoJp sbi) conf =
    B.noticeOfBrokerageAnnouncement sbi connInfo ua $$ sinkText conf
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
            let broker = take 1 [a | a@(Conf.SBIsecCoJp _)<-Conf.brokers conf]
            M.mapM (\b -> fetchUpdatedPriceAndStore b conf) broker `shouldReturn` [()]
    --
    describe "noticeOfCurrentAssets" $
        it "may successful" $ do
            (Right conf) <- Conf.readJSONFile "conf.json"
            let broker = take 1 [a | a@(Conf.SBIsecCoJp _)<-Conf.brokers conf]
            M.mapM (\b -> noticeOfCurrentAssets b conf) broker `shouldReturn` [()]
    --
    describe "noticeOfBrokerageAnnouncement" $
        it "may successful" $ do
            (Right conf) <- Conf.readJSONFile "conf.json"
            let broker = take 1 [a | a@(Conf.SBIsecCoJp _)<-Conf.brokers conf]
            M.mapM (\b -> noticeOfBrokerageAnnouncement b conf) broker `shouldReturn` [()]

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

