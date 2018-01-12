{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
module ConfSpec (spec) where

import           Conf
import           Test.Hspec

--
-- テストファイル
testConfFilePath = "test/conf.test.json"

--
-- テストファイルの内容
confInfo = Info
    { updatePriceMinutes = 10
    , noticeAssetsMinutes = 20
    , userAgent = "enter userAgent"
    , slack = confInfoSlack
    , mariaDB = confInfoMariaDB
    , broker = confInfoBroker
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

confInfoMatsuiCoJp = InfoMatsuiCoJp
    { loginURL  = "enter loginURL"
    , account   = InfoAccount
        { loginID   = "enter loginID"
        , loginPassword = "enter loginPassword"
        , dealingsPassword = "enter dealingsPassword"
        }
    }

confInfoBroker =
    MatsuiCoJp confInfoMatsuiCoJp

--
-- Hspecテスト
spec :: Spec
spec = do
    describe "readJSONFile" $ do
        it "invalid input test, \"file not exist\"" $ do
            readJSONFile "" `shouldThrow` anyIOException
        it ("can parse this file \"" ++ testConfFilePath ++ "\"") $ do
            readJSONFile testConfFilePath `shouldReturn` (Right confInfo)


