{-# LANGUAGE DuplicateRecordFields #-}
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
    , brokers = confInfoBrokers
    }

confInfoSlack = InfoSlack
    { webHookURL = "enter webHookURL"
    , access_token = "enter access token"
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

confInfoBrokers =
    [ MatsuiCoJp . InfoMatsuiCoJp $ InfoAccount
        { loginID   = "matsui.co.jp loginID"
        , loginPassword = "matsui.co.jp loginPassword"
        , dealingsPassword = "matsui.co.jp dealingsPassword"
        }
    , SBIsecCoJp . InfoSBIsecCoJp $ InfoAccount
        { loginID   = "sbisec.co.jp loginID"
        , loginPassword = "sbisec.co.jp loginPassword"
        , dealingsPassword = "sbisec.co.jp dealingsPassword"
        }
    , KabuCom . InfoKabuCom $ InfoAccount
        { loginID   = "kabu.com loginID"
        , loginPassword = "kabu.com loginPassword"
        , dealingsPassword = "kabu.com dealingsPassword"
        }
    ]

--
-- Hspecテスト
spec :: Spec
spec =
    describe "readJSONFile" $ do
        it "invalid input test, \"file not exist\"" $
            readJSONFile "" `shouldThrow` anyIOException
        it ("can parse this file \"" ++ testConfFilePath ++ "\"") $
            readJSONFile testConfFilePath `shouldReturn` Right confInfo


