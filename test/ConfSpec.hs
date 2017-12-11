{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module ConfSpec (spec) where

import qualified Data.Either as Either

import           Conf
import           Test.Hspec

--
-- テストファイル
testConfFilePath = "test/conf.test.json"

--
-- テストファイルの内容
confInfo = Info
    { recordAssetsInterval = 10
    , sendReportInterval = 20
    , loginURL  = "enter loginURL"
    , loginID   = "enter loginID"
    , loginPassword = "enter loginPassword"
    , dealingsPassword = "enter dealingsPassword"
    , userAgent = "enter userAgent"
    , slack = confInfoSlack
    , mariaDB = confInfoMariaDB
    }

confInfoSlack = InfoSlack
    { webHookURL = "enter webHookURL"
    , channel = "enter channel"
    , userName = "enter userName"
    }

confInfoMariaDB = InfoMariaDB
    { host     = "localhost"
    , port     = 3306
    , user     = "enter user"
    , password = "enter password"
    , database = "stockdb"
    }

--
-- Hspecテスト
spec :: Spec
spec = do
    describe "readJSONFile" $ do
        it "invalid input test, \"file not exist\"" $ do
            readJSONFile "" `shouldThrow` anyIOException
        it ("can parse this file \"" ++ testConfFilePath ++ "\"") $ do
            readJSONFile testConfFilePath `shouldReturn` (Right confInfo)
            readJSONFile testConfFilePath >>= print


