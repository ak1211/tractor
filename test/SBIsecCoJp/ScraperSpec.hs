{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module SBIsecCoJp.ScraperSpec (spec) where

import qualified Control.Monad      as M
import qualified Data.Text          as T
import qualified Data.Text.Lazy     as TL
import qualified Data.Text.Lazy.IO  as TL
import           Test.Hspec

import qualified BrokerBackend      as BB
import qualified GenScraper         as GS
import           ModelDef           (TickerSymbol (..))
import qualified SBIsecCoJp.Scraper as S

--
-- マーケット情報ページ
--
test01MarketPage = "https%3A%2F%2Fk.sbisec.co.jp%2Fbsite%2Fmarket%2Fmenu.do.utf8.01.html"
test01MarketPage' = Just $ S.MInikkei225
 $ Just S.MarketInfo
  { S.miPrice = 23631.88
  , S.miMonth = 1
  , S.miDay = 26
  , S.miHour = 15
  , S.miMin = 15
  , S.miDifference = (-37.61)
  , S.miDiffPercent = (-0.16)
  , S.miOpen = 23757.34
  , S.miHigh = 23797.96
  , S.miLow = 23592.28
  }

-- マーケット情報ページ (まだ取引のない場合)
test02MarketPage = "https%3A%2F%2Fk.sbisec.co.jp%2Fbsite%2Fmarket%2Fmenu.do.utf8.02.html"
test02MarketPage' = Just (S.MInikkei225 Nothing)

--
-- ログインページ
--
test01LoginPage = "https%3A%2F%2Fk.sbisec.co.jp%2Fbsite%2Fvisitor%2Ftop.do.utf8.html"
test01LoginPage' = GS.FormTag
 { GS.formAction = "https://k.sbisec.co.jp/bsite/visitor/loginUserCheck.do"
 , GS.formMethod = Just "POST"
 , GS.formInputTag =
  [ GS.InputTag (Just "text") (Just "username") (Just "") GS.Unchecked
  , GS.InputTag (Just "password") (Just "password") (Just "") GS.Unchecked
  , GS.InputTag (Just "submit") (Just "login") (Just "&nbsp;ログイン&nbsp;") GS.Unchecked
  , GS.InputTag (Just "button") (Just "cancel") (Just "キャンセル") GS.Unchecked
  ]
 }

--
-- トップページ
--
test01TopPage = "https%3A%2F%2Fk.sbisec.co.jp%2Fbsite%2Fmember%2Fmenu.do.utf8.html"
test01TopPage' = S.TopPage
 [ GS.AnchorTag {GS.aText="SBI証券　バックアップサイト",GS.aHref="/bsite/member/menu.do"}
 , GS.AnchorTag {GS.aText="ログアウト",GS.aHref="/bsite/member/logout.do"}
 , GS.AnchorTag {GS.aText="取引／株価照会",GS.aHref="/bsite/price/search.do"}
 , GS.AnchorTag {GS.aText="注文照会",GS.aHref="/bsite/member/stock/orderList.do"}
 , GS.AnchorTag {GS.aText="注文取消／訂正",GS.aHref="/bsite/member/stock/orderList.do?cayen.comboOff=1"}
 , GS.AnchorTag {GS.aText="口座管理",GS.aHref="/bsite/member/acc/menu.do"}
 , GS.AnchorTag {GS.aText="保有証券一覧",GS.aHref="/bsite/member/acc/holdStockList.do"}
 , GS.AnchorTag {GS.aText="買付余力",GS.aHref="/bsite/member/acc/purchaseMarginList.do"}
 , GS.AnchorTag {GS.aText="信用建玉一覧",GS.aHref="/bsite/member/acc/positionList.do"}
 , GS.AnchorTag {GS.aText="信用建余力",GS.aHref="/bsite/member/acc/positionMargin.do"}
 , GS.AnchorTag {GS.aText="出金／振替指示",GS.aHref="/bsite/member/banking/withdrawMenu.do"}
 , GS.AnchorTag {GS.aText="ATMカード",GS.aHref="/bsite/member/acc/atmwithdrawMax.do"}
 , GS.AnchorTag {GS.aText="重要なお知らせ",GS.aHref="javascript:openJmsg('/bsite/member/jmsg/JMsgRedirect.do?categoryId=03')"}
 , GS.AnchorTag {GS.aText="当社からのお知らせ",GS.aHref="javascript:openETrade('/bsite/member/jmsg/JMsgRedirect.do?categoryId=02')"}
 , GS.AnchorTag {GS.aText="登録銘柄",GS.aHref="/bsite/member/portfolio/registeredStockList.do"}
 , GS.AnchorTag {GS.aText="銘柄登録",GS.aHref="/bsite/member/portfolio/stockSearch.do"}
 , GS.AnchorTag {GS.aText="銘柄一括登録／編集",GS.aHref="/bsite/member/portfolio/lumpStockEntry.do"}
 , GS.AnchorTag {GS.aText="銘柄削除",GS.aHref="/bsite/member/portfolio/registeredStockDelete.do"}
 , GS.AnchorTag {GS.aText="リスト作成／変更",GS.aHref="/bsite/member/portfolio/listEdit.do"}
 , GS.AnchorTag {GS.aText="マーケット情報",GS.aHref="/bsite/market/menu.do"}
 , GS.AnchorTag {GS.aText="ランキング",GS.aHref="/bsite/market/rankingListM.do"}
 , GS.AnchorTag {GS.aText="海外指標",GS.aHref="/bsite/market/foreignIndexDetail.do"}
 , GS.AnchorTag {GS.aText="外国為替",GS.aHref="/bsite/market/forexDetail.do"}
 , GS.AnchorTag {GS.aText="市況コメント",GS.aHref="/bsite/market/marketInfoList.do"}
 , GS.AnchorTag {GS.aText="ニュース",GS.aHref="/bsite/market/newsList.do"}
 , GS.AnchorTag {GS.aText="取引パスワード／注文確認省略設定",GS.aHref="/bsite/member/setting/omissionSetting.do"}
 , GS.AnchorTag {GS.aText="手数料",GS.aHref="/bsite/info/productGroupList.do?service_info_id=commission"}
 , GS.AnchorTag {GS.aText="取扱商品",GS.aHref="/bsite/info/productGroupList.do?service_info_id=goodsList"}
 , GS.AnchorTag {GS.aText="新着情報/キャンペーン",GS.aHref="/bsite/info/campaignInfoList.do"}
 , GS.AnchorTag {GS.aText="お問い合わせ",GS.aHref="/bsite/info/inquiryList.do"}
 , GS.AnchorTag {GS.aText="ログイン履歴",GS.aHref="/bsite/member/loginHistory.do"}
 , GS.AnchorTag {GS.aText="！ポリシー／免責事項",GS.aHref="/bsite/info/policyList.do"}
 , GS.AnchorTag {GS.aText="システムメンテナンス",GS.aHref="/bsite/info/systemInfoDetail.do?sortation_id=maintenance"}
 , GS.AnchorTag {GS.aText="ログアウト",GS.aHref="/bsite/member/logout.do"}
 , GS.AnchorTag {GS.aText="金融商品取引法に係る表示",GS.aHref="/bsite/info/policyDetail.do?list=title&policy_info_id=salesLaw&text_no=1"}
 ]

--
-- 口座管理ページ
--
test01AccMenuPage = "https%3A%2F%2Fk.sbisec.co.jp%2Fbsite%2Fmember%2Facc%2Fmenu.do.utf8.html"
test01AccMenuPage' = S.AccMenuPage
 [ GS.AnchorTag {GS.aText="SBI証券　バックアップサイト",GS.aHref="/bsite/member/menu.do"}
 , GS.AnchorTag {GS.aText="トップ",GS.aHref="/bsite/member/menu.do"}
 , GS.AnchorTag {GS.aText="ログアウト",GS.aHref="/bsite/member/logout.do"}
 , GS.AnchorTag {GS.aText="取引／株価照会",GS.aHref="/bsite/price/search.do"}
 , GS.AnchorTag {GS.aText="登録銘柄",GS.aHref="/bsite/member/portfolio/registeredStockList.do"}
 , GS.AnchorTag {GS.aText="口座管理",GS.aHref="/bsite/member/acc/menu.do"}
 , GS.AnchorTag {GS.aText="マーケット情報",GS.aHref="/bsite/market/menu.do"}
 , GS.AnchorTag {GS.aText="保有証券一覧",GS.aHref="/bsite/member/acc/holdStockList.do"}
 , GS.AnchorTag {GS.aText="買付余力",GS.aHref="/bsite/member/acc/purchaseMarginList.do"}
 , GS.AnchorTag {GS.aText="信用建玉一覧",GS.aHref="/bsite/member/acc/positionList.do"}
 , GS.AnchorTag {GS.aText="信用建余力",GS.aHref="/bsite/member/acc/positionMargin.do"}
 , GS.AnchorTag {GS.aText="出金／振替指示",GS.aHref="/bsite/member/banking/withdrawMenu.do"}
 , GS.AnchorTag {GS.aText="ATMカード",GS.aHref="/bsite/member/acc/atmMenu.do"}
 , GS.AnchorTag {GS.aText="トップ",GS.aHref="/bsite/member/menu.do"}
 , GS.AnchorTag {GS.aText="ログアウト",GS.aHref="/bsite/member/logout.do"}
 , GS.AnchorTag {GS.aText="金融商品取引法に係る表示",GS.aHref="/bsite/info/policyDetail.do?list=title&policy_info_id=salesLaw&text_no=1"}
 ]

--
-- 買付余力ページ
--
test01AccPurchaseMarginListPage = "https%3A%2F%2Fk.sbisec.co.jp%2Fbsite%2Fmember%2Facc%2FpurchaseMarginList.do.utf8.html"
test01AccPurchaseMarginListPage' = S.PurchaseMarginListPage
 [ GS.AnchorTag {GS.aText="詳細",GS.aHref="/bsite/member/acc/purchaseMarginDetail.do?no=3&account_type=1"}
 , GS.AnchorTag {GS.aText="詳細",GS.aHref="/bsite/member/acc/purchaseMarginDetail.do?no=4&account_type=1"}
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
  [ GS.AnchorTag {GS.aText="ヨロシサン製薬",GS.aHref="/bsite/member/acc/holdStockDetail.do?company_code=1111&new_old_id=+&hitokutei_kbn=0&st_right_id=+&"}
  , GS.AnchorTag {GS.aText="オムラ・インダストリ",GS.aHref="/bsite/member/acc/holdStockDetail.do?company_code=2222&new_old_id=+&hitokutei_kbn=0&st_right_id=+&"}
  ]
}

--
-- 保有証券詳細ページ
--
test01AccHoldStockDetailPage = "https%3A%2F%2Fk.sbisec.co.jp%2Fbsite%2Fmember%2Facc%2FholdStockDetail.do%3Fcompany_code%3D1111.utf8.01.html"
test01AccHoldStockDetailPage' = Just S.HoldStockDetailPage
 { S.hsdUserid = "Z12-1234567"
 , hsdMDHourMin = Just (1,22,15,00)
 , hsdTicker = TSTYO 1111
 , hsdCaption = "ヨロシサン製薬"
 , hsdDiff = Just (-1)
 , hsdCount = 100
 , hsdPurchasePrice = 100
 , hsdPrice = 120
}

-- 保有証券詳細ページ(まだ取引のない場合)
test02AccHoldStockDetailPage = "https%3A%2F%2Fk.sbisec.co.jp%2Fbsite%2Fmember%2Facc%2FholdStockDetail.do%3Fcompany_code%3D1111.utf8.02.html"
test02AccHoldStockDetailPage' = Just S.HoldStockDetailPage
 { S.hsdUserid = "Z12-1234567"
 , hsdMDHourMin = Nothing
 , hsdTicker = TSTYO 1111
 , hsdCaption = "ヨロシサン製薬"
 , hsdDiff = Nothing
 , hsdCount = 100
 , hsdPurchasePrice = 100
 , hsdPrice = 120
}

test03AccHoldStockDetailPage = "https%3A%2F%2Fk.sbisec.co.jp%2Fbsite%2Fmember%2Facc%2FholdStockDetail.do%3Fcompany_code%3D2222.utf8.html"
test03AccHoldStockDetailPage' = Just S.HoldStockDetailPage
 { S.hsdUserid = "Z12-1234567"
 , hsdMDHourMin = Just (1,22,15,00)
 , hsdTicker = TSTYO 2222
 , hsdCaption = "オムラ・インダストリ"
 , hsdDiff = Just 100
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
    describe "marketInfoPage" $ do
        it "https://k.sbisec.co.jp/bsite/market/menu.do 01" $ do
            html <- TL.readFile ("test/SBIsecCoJp/" ++ test01MarketPage)
            S.marketInfoPage html `shouldBe` test01MarketPage'
        it "https://k.sbisec.co.jp/bsite/market/menu.do 02" $ do
            html <- TL.readFile ("test/SBIsecCoJp/" ++ test02MarketPage)
            S.marketInfoPage html `shouldBe` test02MarketPage'
    --
    describe "formLoginPage" $
        it "https://k.sbisec.co.jp/bsite/visitor/top.do" $ do
            html <- TL.readFile ("test/SBIsecCoJp/" ++ test01LoginPage)
            S.formLoginPage html >>= shouldBe test01LoginPage'
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
        it "https://k.sbisec.co.jp/bsite/member/acc/holdStockDetail.do?company_code=1111 01" $ do
            html <- TL.readFile ("test/SBIsecCoJp/" ++ test01AccHoldStockDetailPage)
            S.holdStockDetailPage html `shouldBe` test01AccHoldStockDetailPage'
        it "https://k.sbisec.co.jp/bsite/member/acc/holdStockDetail.do?company_code=1111 02" $ do
            html <- TL.readFile ("test/SBIsecCoJp/" ++ test02AccHoldStockDetailPage)
            S.holdStockDetailPage html `shouldBe` test02AccHoldStockDetailPage'
        it "https://k.sbisec.co.jp/bsite/member/acc/holdStockDetail.do?company_code=2222" $ do
            html <- TL.readFile ("test/SBIsecCoJp/" ++ test03AccHoldStockDetailPage)
            S.holdStockDetailPage html `shouldBe` test03AccHoldStockDetailPage'
--            print $ S.holdStockDetailPage html
--            M.mapM_ (TL.putStrLn . TL.fromStrict {-. T.unwords-}) $ S.holdStockDetailPage html

