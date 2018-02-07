{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module SBIsecCoJp.ScraperSpec (spec) where

import qualified Control.Monad      as M
import qualified Data.Text          as T
import qualified Data.Text.Lazy     as TL
import qualified Data.Text.Lazy.IO  as TL
import           Test.Hspec

import qualified BrokerBackend      as BB
import           ModelDef           (TickerSymbol (..))
import qualified SBIsecCoJp.Scraper as S
import qualified ScraperBackend     as SB

--
-- マーケット情報ページ
--
test01MarketPage = "https%3A%2F%2Fk.sbisec.co.jp%2Fbsite%2Fmarket%2Fmenu.do.utf8.html"
test01MarketPage' = Just S.MarketInfoPage
 { S.miCaption = "国内指標"
 , S.miPrice = Just 23631.88
 , S.miMonth = 1
 , S.miDay = 26
 , S.miHour = 15
 , S.miMinute = 15
 , S.miDifference = Just (-37.61)
 , S.miDiffPercent = Just (-0.16)
 , S.miOpen = Just 23757.34
 , S.miHigh = Just 23797.96
 , S.miLow = Just 23592.28
 }

--
-- ログインページ
--
test01LoginPage = "https%3A%2F%2Fk.sbisec.co.jp%2Fbsite%2Fvisitor%2Ftop.do.utf8.html"
test01LoginPage' = SB.FormTag
 { SB.formAction = "https://k.sbisec.co.jp/bsite/visitor/loginUserCheck.do"
 , SB.formMethod = Just "POST"
 , SB.formInputTag =
  [ SB.InputTag (Just "text") (Just "username") (Just "")
  , SB.InputTag (Just "password") (Just "password") (Just "")
  , SB.InputTag (Just "submit") (Just "login") (Just "&nbsp;ログイン&nbsp;")
  , SB.InputTag (Just "button") (Just "cancel") (Just "キャンセル")
  ]
 }

--
-- トップページ
--
test01TopPage = "https%3A%2F%2Fk.sbisec.co.jp%2Fbsite%2Fmember%2Fmenu.do.utf8.html"
test01TopPage' = S.TopPage
 [ ("ログアウト","/bsite/member/logout.do")
 , ("取引／株価照会","/bsite/price/search.do")
 , ("注文照会","/bsite/member/stock/orderList.do")
 , ("注文取消／訂正","/bsite/member/stock/orderList.do?cayen.comboOff=1")
 , ("口座管理","/bsite/member/acc/menu.do")
 , ("保有証券一覧","/bsite/member/acc/holdStockList.do")
 , ("買付余力","/bsite/member/acc/purchaseMarginList.do")
 , ("信用建玉一覧","/bsite/member/acc/positionList.do")
 , ("信用建余力","/bsite/member/acc/positionMargin.do")
 , ("出金／振替指示","/bsite/member/banking/withdrawMenu.do")
 , ("ATMカード","/bsite/member/acc/atmwithdrawMax.do")
 , ("重要なお知らせ","javascript:openJmsg('/bsite/member/jmsg/JMsgRedirect.do?categoryId=03')")
 , ("当社からのお知らせ","javascript:openETrade('/bsite/member/jmsg/JMsgRedirect.do?categoryId=02')")
 , ("登録銘柄","/bsite/member/portfolio/registeredStockList.do")
 , ("銘柄登録","/bsite/member/portfolio/stockSearch.do")
 , ("銘柄一括登録／編集","/bsite/member/portfolio/lumpStockEntry.do")
 , ("銘柄削除","/bsite/member/portfolio/registeredStockDelete.do")
 , ("リスト作成／変更","/bsite/member/portfolio/listEdit.do")
 , ("マーケット情報","/bsite/market/menu.do")
 , ("ランキング","/bsite/market/rankingListM.do")
 , ("海外指標","/bsite/market/foreignIndexDetail.do")
 , ("外国為替","/bsite/market/forexDetail.do")
 , ("市況コメント","/bsite/market/marketInfoList.do")
 , ("ニュース","/bsite/market/newsList.do")
 , ("取引パスワード／注文確認省略設定","/bsite/member/setting/omissionSetting.do")
 , ("手数料","/bsite/info/productGroupList.do?service_info_id=commission")
 , ("取扱商品","/bsite/info/productGroupList.do?service_info_id=goodsList")
 , ("新着情報/キャンペーン","/bsite/info/campaignInfoList.do")
 , ("お問い合わせ","/bsite/info/inquiryList.do")
 , ("ログイン履歴","/bsite/member/loginHistory.do")
 , ("！ポリシー／免責事項","/bsite/info/policyList.do")
 , ("システムメンテナンス","/bsite/info/systemInfoDetail.do?sortation_id=maintenance")
 , ("ログアウト","/bsite/member/logout.do")
 , ("金融商品取引法に係る表示","/bsite/info/policyDetail.do?list=title&policy_info_id=salesLaw&text_no=1")
 ]

--
-- 口座管理ページ
--
test01AccMenuPage = "https%3A%2F%2Fk.sbisec.co.jp%2Fbsite%2Fmember%2Facc%2Fmenu.do.utf8.html"
test01AccMenuPage' = S.AccMenuPage
 [ ("トップ","/bsite/member/menu.do")
 , ("ログアウト","/bsite/member/logout.do")
 , ("取引／株価照会","/bsite/price/search.do")
 , ("登録銘柄","/bsite/member/portfolio/registeredStockList.do")
 , ("口座管理","/bsite/member/acc/menu.do")
 , ("マーケット情報","/bsite/market/menu.do")
 , ("保有証券一覧","/bsite/member/acc/holdStockList.do")
 , ("買付余力","/bsite/member/acc/purchaseMarginList.do")
 , ("信用建玉一覧","/bsite/member/acc/positionList.do")
 , ("信用建余力","/bsite/member/acc/positionMargin.do")
 , ("出金／振替指示","/bsite/member/banking/withdrawMenu.do")
 , ("ATMカード","/bsite/member/acc/atmMenu.do")
 , ("トップ","/bsite/member/menu.do")
 , ("ログアウト","/bsite/member/logout.do")
 , ("金融商品取引法に係る表示","/bsite/info/policyDetail.do?list=title&policy_info_id=salesLaw&text_no=1")
 ]

--
-- 買付余力ページ
--
test01AccPurchaseMarginListPage = "https%3A%2F%2Fk.sbisec.co.jp%2Fbsite%2Fmember%2Facc%2FpurchaseMarginList.do.utf8.html"
test01AccPurchaseMarginListPage' = S.PurchaseMarginListPage
 [ ("詳細","/bsite/member/acc/purchaseMarginDetail.do?no=3&account_type=1")
 , ("詳細","/bsite/member/acc/purchaseMarginDetail.do?no=4&account_type=1")
 ]

--
-- 買付余力詳細ページ
--
test01AccPurchaseMarginDetailPage = "https%3A%2F%2Fk.sbisec.co.jp%2Fbsite%2Fmember%2Facc%2FpurchaseMarginDetail.do%3Fno%3D3%26account_type%3D1.utf8.html"
test01AccPurchaseMarginDetailPage' = Just S.PurchaseMarginDetailPage
 { pmdUserid = "Z12-1234567"
 , pmdDay = "2018/01/26"
 , pmdMoneyToSpare = 123456
 , pmdCashBalance = 321654
 }

--
-- 保有証券一覧ページ
--
test01AccHoldStockListPage = "https%3A%2F%2Fk.sbisec.co.jp%2Fbsite%2Fmember%2Facc%2FholdStockList.do.utf8.html"
test01AccHoldStockListPage' = Just S.HoldStockListPage
 { S.hslUserid = "Z12-1234567"
 , S.hslNthPages = "1～2件(2件中)"
 , S.hslEvaluation = 123456.0
 , S.hslProfit = 1500.0
 , S.hslLinks = S.HoldStockDetailLink
  [ ("ヨロシサン製薬","/bsite/member/acc/holdStockDetail.do?company_code=1111&new_old_id=+&hitokutei_kbn=0&st_right_id=+&")
  , ("オムラ・インダストリ","/bsite/member/acc/holdStockDetail.do?company_code=2222&new_old_id=+&hitokutei_kbn=0&st_right_id=+&")
  ]
}

--
-- 保有証券詳細ページ
--
test01AccHoldStockDetailPage = "https%3A%2F%2Fk.sbisec.co.jp%2Fbsite%2Fmember%2Facc%2FholdStockDetail.do%3Fcompany_code%3D1111.utf8.html"
test01AccHoldStockDetailPage' = Just S.HoldStockDetailPage
 { S.hsdUserid = "Z12-1234567"
 , hsdTicker = TSTYO 1111
 , hsdCaption = "ヨロシサン製薬"
 , hsdDiff = -1
 , hsdCount = 100
 , hsdPurchasePrice = 100
 , hsdPrice = 120
}

test02AccHoldStockDetailPage = "https%3A%2F%2Fk.sbisec.co.jp%2Fbsite%2Fmember%2Facc%2FholdStockDetail.do%3Fcompany_code%3D2222.utf8.html"
test02AccHoldStockDetailPage' = Just S.HoldStockDetailPage
 { S.hsdUserid = "Z12-1234567"
 , hsdTicker = TSTYO 2222
 , hsdCaption = "オムラ・インダストリ"
 , hsdDiff = 100
 , hsdCount = 100
 , hsdPurchasePrice = 1000
 , hsdPrice = 1035
}

--
-- Hspecテスト
--
spec :: Spec
spec = do
    --
    describe "marketInfoPage" $
        it "https://k.sbisec.co.jp/bsite/market/menu.do" $ do
            html <- TL.readFile ("test/SBIsecCoJp/" ++ test01MarketPage)
            S.marketInfoPage html `shouldBe` test01MarketPage'
    --
    describe "formLoginPage" $
        it "https://k.sbisec.co.jp/bsite/visitor/top.do" $ do
            html <- TL.readFile ("test/SBIsecCoJp/" ++ test01LoginPage)
            case S.formLoginPage html of
                Left l  -> TL.putStrLn l
                Right r -> r `shouldBe` test01LoginPage'
    --
    describe "topPage" $
        it "https://k.sbisec.co.jp/bsite/member/menu.do" $ do
            html <- TL.readFile ("test/SBIsecCoJp/" ++ test01TopPage)
            S.topPage html `shouldBe` test01TopPage'
    --
    describe "accMenuPage" $
        it "https://k.sbisec.co.jp/bsite/member/acc/menu.do" $ do
            html <- TL.readFile ("test/SBIsecCoJp/" ++ test01AccMenuPage)
            S.accMenuPage html `shouldBe` test01AccMenuPage'
    --
    describe "purchaseMarginListPage" $
        it "https://k.sbisec.co.jp/bsite/member/acc/purchaseMarginList.do" $ do
            html <- TL.readFile ("test/SBIsecCoJp/" ++ test01AccPurchaseMarginListPage)
            S.purchaseMarginListPage html `shouldBe` test01AccPurchaseMarginListPage'
    --
    describe "purchaseMarginDetailPage" $
        it "https://k.sbisec.co.jp/bsite/member/acc/purchaseMarginDetail.do?no=3&account_type=1" $ do
            html <- TL.readFile ("test/SBIsecCoJp/" ++ test01AccPurchaseMarginDetailPage)
            S.purchaseMarginDetailPage html `shouldBe` test01AccPurchaseMarginDetailPage'
    --
    describe "holdStockListPage" $
        it "https://k.sbisec.co.jp/bsite/member/acc/holdStockList.do" $ do
            html <- TL.readFile ("test/SBIsecCoJp/" ++ test01AccHoldStockListPage)
            S.holdStockListPage html `shouldBe` test01AccHoldStockListPage'
    --
    describe "acc/holdStockDetail page" $ do
        it "https://k.sbisec.co.jp/bsite/member/acc/holdStockDetail.do?company_code=1111" $ do
            html <- TL.readFile ("test/SBIsecCoJp/" ++ test01AccHoldStockDetailPage)
            S.holdStockDetailPage html `shouldBe` test01AccHoldStockDetailPage'
        it "https://k.sbisec.co.jp/bsite/member/acc/holdStockDetail.do?company_code=2222" $ do
            html <- TL.readFile ("test/SBIsecCoJp/" ++ test02AccHoldStockDetailPage)
            S.holdStockDetailPage html `shouldBe` test02AccHoldStockDetailPage'
--            print $ S.holdStockDetailPage html
--            M.mapM_ (TL.putStrLn . TL.fromStrict {-. T.unwords-}) $ S.holdStockDetailPage html

