{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module KabuCom.ScraperSpec (spec) where

import           Control.Exception.Safe
import qualified Control.Monad          as M
import           Data.Maybe             (fromJust)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.IO      as TL
import qualified Data.Time              as Tm
import           Test.Hspec

import           GenScraper
import           KabuCom.Scraper
import           ModelDef               (TickerSymbol (..))
import           Scheduling             (AsiaTokyoDay (..))

--
-- ログインページ
--
test01LoginPage = "https%3A%2F%2Fs10.kabu.co.jp%2F_mem_bin%2Flight%2Flogin.asp%3F%2Flight.utf8.html"
test01LoginPage' = FormTag
 { formAction = "https://s10.kabu.co.jp/_mem_bin/Light/verifpwd.asp"
 , formMethod = Just "POST"
 , formInputTag =
  [ InputTag (Just "text") (Just "SsLogonUser") (Just "") Unchecked
  , InputTag (Just "PASSWORD") (Just "SsLogonPassword") Nothing Unchecked
  , InputTag (Just "radio") (Just "SsLogonHost") (Just "100") Unchecked
  , InputTag (Just "radio") (Just "SsLogonHost") (Just "200") Checked
  , InputTag (Just "hidden") (Just "SsLoginPage") (Just "/light/") Unchecked
  , InputTag (Just "submit") (Just "submit1") (Just "ログイン") Unchecked
  , InputTag (Just "checkbox") (Just "CookieOn") (Just "1") Checked
  ]
 }

--
-- トップメニューページ
--
test01TopPage = "https%3A%2F%2Fs20.si0.kabu.co.jp%2FLight.utf8.html"
test01TopPage' = TopPage
 [ AnchorTag {aText="TOP MENU",aHref="https://s20.si0.kabu.co.jp/Light/Default.asp"}
 , AnchorTag {aText="NEWS",aHref="https://s20.si0.kabu.co.jp/Light/Personal/pinpointnews/newslist.asp?ConfirmFlg=0&clrRecSet=1"}
 , AnchorTag {aText="LOG OUT",aHref="https://s20.si0.kabu.co.jp/Members/LogOut.asp"}
 , AnchorTag {aText="お客さまへのお知らせ",aHref="Personal/pinpointnews/newslist.asp?ConfirmFlg=0&clrRecSet=1"}
 , AnchorTag {aText="お取引関連通知",aHref="Personal/PersonalNews.asp"}
 , AnchorTag {aText="買注文",aHref="/ap/light/Stocks/Stock/Search/ByKeyword"}
 , AnchorTag {aText="売注文",aHref="/ap/light/Stocks/Stock/Position/SellList"}
 , AnchorTag {aText="注文訂正・取消",aHref="/ap/light/Stocks/Stock/OrderStatus/OrderSelectList"}
 , AnchorTag {aText="特定⇒一般口座振替",aHref="../Light/Trade/KabuSPKouza/KSK1101.asp"}
 , AnchorTag {aText="kabu.comからのお知らせ",aHref="KabucomNews.asp"}
 , AnchorTag {aText="kabu.board",aHref="FavoriteBoard.asp"}
 , AnchorTag {aText="先物・オプションボード",aHref="../Light/TradeTool/DerivBoard.asp"}
 , AnchorTag {aText="口座開設申込",aHref="../Light/Personal/ShinyouMoushikomi/SMRule.asp"}
 , AnchorTag {aText="口座開設申込",aHref="../Light/Personal/OptionMoushikomi/OMRule.asp"}
 , AnchorTag {aText="買注文",aHref="../Light/Trade/PetitBuy/BPetitSelect.asp"}
 , AnchorTag {aText="売注文",aHref="../Light/Trade/PetitSell/SPetitSelect.asp"}
 , AnchorTag {aText="注文取消",aHref="../Light/Trade/PetitEdit/EPetitSelect.asp"}
 , AnchorTag {aText="単元株化取引",aHref="../Light/Trade/PetitSell/BSPetitSelect.asp"}
 , AnchorTag {aText="MMF・中国Fメニュー",aHref="../Light/Trade/MMFSelect.asp"}
 , AnchorTag {aText="投信メニュー",aHref="../Light/Trade/ToushinSelect.asp"}
 , AnchorTag {aText="積立メニュー",aHref="../light/trade/teikikaitsuke/tsumitatemenu.asp"}
 , AnchorTag {aText="入金依頼",aHref="Trade/CashIn/CILIST.asp"}
 , AnchorTag {aText="出金依頼",aHref="Trade/CashOut/CO01101.asp"}
 , AnchorTag {aText="入出金予定",aHref="Account/CashPlan/cashReserve.asp"}
 , AnchorTag {aText="KabuCall",aHref="https://s20.si0.kabu.co.jp/ap/light/Notify/KabuCall/Stock/Input"}
 , AnchorTag {aText="銘柄検索(四季報)",aHref="https://s20.si0.kabu.co.jp/Light/TradeTool/StockSearch.asp"}
 , AnchorTag {aText="スクリーニング",aHref="https://s20.si0.kabu.co.jp/Light/TradeTool/ScreeningMenu.asp"}
 , AnchorTag {aText="証拠金シミュレーター",aHref="https://s20.si0.kabu.co.jp/Light/TradeTool/MarginSimulator/MarginSimulator.asp"}
 , AnchorTag {aText="QUICK リサーチネット",aHref="https://s20.si0.kabu.co.jp/Light/TradeTool/ResearchNet/ResearchNet.asp"}
 , AnchorTag {aText="株式新聞速報ニュース",aHref="https://s20.si0.kabu.co.jp/Light/TradeTool/KabuNews/"}
 , AnchorTag {aText="先物オプション速報ニュース",aHref="../Light/TradeTool/FoNews/"}
 , AnchorTag {aText="kabu.com投資情報室",aHref="https://s20.si0.kabu.co.jp/Light/TradeTool/IGOffice/"}
 , AnchorTag {aText="売買用語解説",aHref="https://s20.si0.kabu.co.jp/Light/TradeTool/Baibai.asp"}
 , AnchorTag {aText="注文約定照会",aHref="/ap/Light/Stocks/Stock/OrderStatus/List"}
 , AnchorTag {aText="取引履歴",aHref="/ap/Light/Stocks/Stock/History/List"}
 , AnchorTag {aText="預り資産評価",aHref="../Light/Account/AssetReviewEx/AssetReview.asp"}
 , AnchorTag {aText="預り資産推移",aHref="../Light/Account/AssetTransition/AssetTransition.asp"}
 , AnchorTag {aText="残高照会",aHref="/ap/light/Stocks/Stock/Position/List"}
 , AnchorTag {aText="入出金確認",aHref="Account/CashStatus/CashStatus.asp"}
 , AnchorTag {aText="ポートフォリオ診断",aHref="../Light/Account/PortFolio/PortFolio.asp"}
 , AnchorTag {aText="買付出金可能額",aHref="/ap/light/Assets/Kanougaku/Stock"}
 , AnchorTag {aText="建玉可能額・買付可能額",aHref="/ap/light/Assets/Kanougaku/Margin"}
 , AnchorTag {aText="先物・オプション建玉可能額",aHref="/ap/light/Assets/Kanougaku/Deriv"}
 , AnchorTag {aText="不足保証金自動振替サービス状況",aHref="../Light/Account/RealDebit/RealDebitSecurity.asp"}
 , AnchorTag {aText="お客様情報",aHref="Personal/Profile.asp"}
 , AnchorTag {aText="買付可能残高表示",aHref="Personal/btmuzandaka/BZ01101.asp"}
 , AnchorTag {aText="らくらく電子契約",aHref="Personal/DKStatus/DK01101.asp"}
 , AnchorTag {aText="サービス状態",aHref="Support/Sysprop/SysProp.asp"}
 , AnchorTag {aText="お問い合わせ",aHref="Support/Menu.asp"}
 , AnchorTag {aText="取引制限銘柄",aHref="Support/LimitedStock/LimitedStock.asp"}
 , AnchorTag {aText="◆ご投資にかかる手数料等およびリスクについて",aHref="/light/escapeclause.asp"}
 ]

--
-- 買付出金可能額ページ
--
test01PurchaseMarginPage = "https%3A%2F%2Fs20.si0.kabu.co.jp%2Fap%2Flight%2FAssets%2FKanougaku%2FStock.utf8.html"
test01PurchaseMarginPage' = PurchaseMarginPage
 { pmMoneyToSpare = 1024
 , pmCashBalance = 2048
 }

--
-- 残高照会ページ
--
test01StockPositionListPage = "https%3A%2F%2Fs20.si0.kabu.co.jp%2Fap%2Flight%2FStocks%2FStock%2FPosition%2FList.utf8.html"
test01StockPositionListPage' = StockPositionListPage
 { splCaption = "国内株式(特定預り)"
 , splMDHourMin = (2,19,23,35)
 , splEvaluation = 134410
 , splProfit = -3820
 , splProfitPc = -2.8
 , splPositions =
  [ StockPositionItem
   { spDetailAnchor = AnchorTag {aText="",aHref="/ap/Light/Stocks/Stock/Position/List?actType=TOKUTEI&price=Acquisition&current=Exchange&filter=1343&listMode=SummaryAndDetail&pageSize=20&symbol=&pageNo=1"}
   , spCaptionAnchor = AnchorTag {aText="ＲＥＩＴ－ＥＴＦ／ＥＴＦ",aHref="https://s20.si0.kabu.co.jp/Light/TradeTool/StockDetail.asp?StockCode=1343&Market=1"}
   , spTicker = TSTYO 1343
   , spCount = 10
   , spPrice = 1787
   , spPurchasePrice = 1971
   , spAmountPrice = 17870
   , spAmountPurchasePrice = 19710
   , spProfit = -1840
   , spProfitPc = -9.3
   , spSellOrderAnchor = AnchorTag {aText="売却",aHref="/ap/Light/Stocks/Stock/Sell/Input?symbol=1343&exchange=TSE&actType=TOKUTEI&price=Acquisition"}
   , spBuyOrderAnchor = AnchorTag {aText="買付",aHref="/ap/Light/Stocks/Stock/Buy/Input?symbol=1343&exchange=TSE"}
   }
  , StockPositionItem
   { spDetailAnchor = AnchorTag {aText="",aHref="/ap/Light/Stocks/Stock/Position/List?actType=TOKUTEI&price=Acquisition&current=Exchange&filter=1540&listMode=SummaryAndDetail&pageSize=20&symbol=&pageNo=1"}
   , spCaptionAnchor = AnchorTag {aText="純金信託／ＥＴＦ（Ｍ）",aHref="https://s20.si0.kabu.co.jp/Light/TradeTool/StockDetail.asp?StockCode=1540&Market=1"}
   , spTicker = TSTYO 1540
   , spCount = 2
   , spPrice = 4480
   , spPurchasePrice = 4310
   , spAmountPrice = 8960
   , spAmountPurchasePrice = 8620
   , spProfit = 340
   , spProfitPc = 3.9
   , spSellOrderAnchor = AnchorTag {aText="売却",aHref="/ap/Light/Stocks/Stock/Sell/Input?symbol=1540&exchange=TSE&actType=TOKUTEI&price=Acquisition"}
   , spBuyOrderAnchor = AnchorTag {aText="買付",aHref="/ap/Light/Stocks/Stock/Buy/Input?symbol=1540&exchange=TSE"}
   }
  , StockPositionItem
   { spDetailAnchor = AnchorTag {aText="",aHref="/ap/Light/Stocks/Stock/Position/List?actType=TOKUTEI&price=Acquisition&current=Exchange&filter=6911&listMode=SummaryAndDetail&pageSize=20&symbol=&pageNo=1"}
   , spCaptionAnchor = AnchorTag {aText="新日無",aHref="https://s20.si0.kabu.co.jp/Light/TradeTool/StockDetail.asp?StockCode=6911&Market=1"}
   , spTicker = TSTYO 6911
   , spCount = 100
   , spPrice = 873
   , spPurchasePrice = 902
   , spAmountPrice = 87300
   , spAmountPurchasePrice = 90200
   , spProfit = -2900
   , spProfitPc = -3.2
   , spSellOrderAnchor = AnchorTag {aText="売却",aHref="/ap/Light/Stocks/Stock/Sell/Input?symbol=6911&exchange=TSE&actType=TOKUTEI&price=Acquisition"}
   , spBuyOrderAnchor = AnchorTag {aText="買付",aHref="/ap/Light/Stocks/Stock/Buy/Input?symbol=6911&exchange=TSE"}
   }
  , StockPositionItem
   { spDetailAnchor = AnchorTag {aText="",aHref="/ap/Light/Stocks/Stock/Position/List?actType=TOKUTEI&price=Acquisition&current=Exchange&filter=8411&listMode=SummaryAndDetail&pageSize=20&symbol=&pageNo=1"}
   , spCaptionAnchor = AnchorTag {aText="みずほ",aHref="https://s20.si0.kabu.co.jp/Light/TradeTool/StockDetail.asp?StockCode=8411&Market=1"}
   , spTicker = TSTYO 8411
   , spCount = 100
   , spPrice = 202.8
   , spPurchasePrice = 197
   , spAmountPrice = 20280
   , spAmountPurchasePrice = 19700
   , spProfit = 580
   , spProfitPc = 2.9
   , spSellOrderAnchor = AnchorTag {aText="売却",aHref="/ap/Light/Stocks/Stock/Sell/Input?symbol=8411&exchange=TSE&actType=TOKUTEI&price=Acquisition"}
   , spBuyOrderAnchor = AnchorTag {aText="買付",aHref="/ap/Light/Stocks/Stock/Buy/Input?symbol=8411&exchange=TSE"}
   }
  ]
 }

--
-- 個別銘柄の詳細ページ
--
test01StockDetailPage = "https%3A%2F%2Fs20.si0.kabu.co.jp%2FLight%2FTradeTool%2FStockDetail.asp%3FStockCode=1343%26Market=1.utf8.html"
test01StockDetailPage' = StockDetailPage
 { sdpChartLink = AnchorTag {aText="チャート",aHref="https://s20.si0.kabu.co.jp/Light/tradetool/reuters/stockchart.asp?chart=4&StockCode=1343&Market=1"}
 , sdpBoardLink = AnchorTag {aText="板",aHref="https://s20.si0.kabu.co.jp/Light/tradetool/reuters/stockboardframe.asp?StockCode=1343&Market=1"}
 , sdpBuyLink = AnchorTag {aText="",aHref="https://s20.si0.kabu.co.jp/ap/Light/Stocks/Stock/Buy/Input?symbol=1343&exchange=1"}
 , sdpSellLink = AnchorTag {aText="",aHref="https://s20.si0.kabu.co.jp/ap/light/Stocks/Stock/Position/SellList?symbol=1343&FilterType=1"}
 , sdpPetitBuyLink = Just AnchorTag {aText="",aHref="https://s20.si0.kabu.co.jp/Light/Trade/PetitBuy/BP01101.asp?StockCode=1343&Market=20"}
 , sdpPetitSellLink = Just AnchorTag {aText="",aHref="https://s20.si0.kabu.co.jp/Light/Trade/PetitSell/SPetitSelect.asp?Brand=1343&FilterType=1&Market=20"}
 , sdpAskPrice = 18110
 , sdpTicker = TSTYO 1343
 , sdpClass = SC貸借銘柄
 , sdpCaption = "ＮＥＸＴＦＵＮＤＳ東証ＲＥＩＴ指数連動型上場投信"
 , sdpDay = parseAsiaTokyoDay "2018/02/23"
 , sdpPrice = Just (1811,HourMinute (15,00))
 , sdpDiff = Just 17
 , sdpDiffPc = Just 0.94
 , sdpCloseYesterday = Just 1794
 , sdpOpen = Just (1795,HourMinute (9,00))
 , sdpHigh = Just (1813,HourMinute (14,43))
 , sdpLow = Just (1795,HourMinute (09,00))
 , sdpOpenAfternoon = Just (1806, HourMinute (12,30))
 , sdpStdPrice = Just (1811,DayHourMinute (parseAsiaTokyoDay "2018/02/23",HourMinute (15,00)))
 , sdpDailyHistories =
  [ DailyStockPrice {dspDay=parseAsiaTokyoDay "2018/02/22",dspOpen=Just 1795.0,dspHigh=Just 1804.0,dspLow=Just 1788.0,dspClose=Just 1794.0,dspDiff=Just (-8.0),dspVolume=96020}
  , DailyStockPrice {dspDay=parseAsiaTokyoDay "2018/02/21",dspOpen=Just 1796.0,dspHigh=Just 1807.0,dspLow=Just 1792.0,dspClose=Just 1802.0,dspDiff=Just (-5.0),dspVolume=40830}
  , DailyStockPrice {dspDay=parseAsiaTokyoDay "2018/02/20",dspOpen=Just 1791.0,dspHigh=Just 1808.0,dspLow=Just 1787.0,dspClose=Just 1807.0,dspDiff=Just 20.0,dspVolume=53570}
  , DailyStockPrice {dspDay=parseAsiaTokyoDay "2018/02/19",dspOpen=Just 1779.0,dspHigh=Just 1787.0,dspLow=Just 1771.0,dspClose=Just 1787.0,dspDiff=Just 21.0,dspVolume=20250}
  , DailyStockPrice {dspDay=parseAsiaTokyoDay "2018/02/16",dspOpen=Just 1760.0,dspHigh=Just 1773.0,dspLow=Just 1755.0,dspClose=Just 1766.0,dspDiff=Just 17.0,dspVolume=48900}
  ]
 }

test02StockDetailPage = "https%3A%2F%2Fs20.si0.kabu.co.jp%2FLight%2FTradeTool%2FStockDetail.asp%3FStockCode=1540%26Market=1.utf8.html"
test02StockDetailPage' = StockDetailPage
 { sdpChartLink = AnchorTag {aText="チャート",aHref="https://s20.si0.kabu.co.jp/Light/tradetool/reuters/stockchart.asp?chart=4&StockCode=1540&Market=1"}
 , sdpBoardLink = AnchorTag {aText="板",aHref="https://s20.si0.kabu.co.jp/Light/tradetool/reuters/stockboardframe.asp?StockCode=1540&Market=1"}
 , sdpBuyLink = AnchorTag {aText="",aHref="https://s20.si0.kabu.co.jp/ap/Light/Stocks/Stock/Buy/Input?symbol=1540&exchange=1"}
 , sdpSellLink = AnchorTag {aText="",aHref="https://s20.si0.kabu.co.jp/ap/light/Stocks/Stock/Position/SellList?symbol=1540&FilterType=1"}
 , sdpPetitBuyLink = Nothing
 , sdpPetitSellLink = Nothing
 , sdpAskPrice = 4430
 , sdpTicker = TSTYO 1540
 , sdpClass = SC貸借銘柄
 , sdpCaption = "純金上場信託（現物国内保管型）"
 , sdpDay = parseAsiaTokyoDay "2018/02/23"
 , sdpPrice = Just (4430,HourMinute (15,00))
 , sdpDiff = Just 10
 , sdpDiffPc = Just 0.22
 , sdpCloseYesterday = Just 4420
 , sdpOpen = Just (4420,HourMinute (09,00))
 , sdpHigh = Just (4430,HourMinute (09,05))
 , sdpLow = Just (4415,HourMinute (09,01))
 , sdpOpenAfternoon = Just (4425, HourMinute (12,30))
 , sdpStdPrice = Just (4430,DayHourMinute (parseAsiaTokyoDay "2018/02/23",HourMinute (15,00)))
 , sdpDailyHistories =
  [ DailyStockPrice {dspDay=parseAsiaTokyoDay "2018/02/22",dspOpen=Just 4455,dspHigh=Just 4455,dspLow=Just 4420,dspClose=Just 4420,dspDiff=Just (-35),dspVolume=55286}
  , DailyStockPrice {dspDay=parseAsiaTokyoDay "2018/02/21",dspOpen=Just 4460,dspHigh=Just 4470,dspLow=Just 4455,dspClose=Just 4455,dspDiff=Just (-10),dspVolume=31286}
  , DailyStockPrice {dspDay=parseAsiaTokyoDay "2018/02/20",dspOpen=Just 4485,dspHigh=Just 4485,dspLow=Just 4465,dspClose=Just 4465,dspDiff=Just (-15),dspVolume=28807}
  , DailyStockPrice {dspDay=parseAsiaTokyoDay "2018/02/19",dspOpen=Just 4475,dspHigh=Just 4480,dspLow=Just 4470,dspClose=Just 4480,dspDiff=Just (-10),dspVolume=34102}
  , DailyStockPrice {dspDay=parseAsiaTokyoDay "2018/02/16",dspOpen=Just 4490,dspHigh=Just 4495,dspLow=Just 4475,dspClose=Just 4490,dspDiff=Just 0,dspVolume=26932}
  ]
 }

test03StockDetailPage = "https%3A%2F%2Fs20.si0.kabu.co.jp%2FLight%2FTradeTool%2FStockDetail.asp%3FStockCode=8411%26Market=1.utf8.01.html"
test03StockDetailPage' = StockDetailPage
 { sdpChartLink = AnchorTag {aText="チャート",aHref="https://s20.si0.kabu.co.jp/Light/tradetool/reuters/stockchart.asp?chart=4&StockCode=8411&Market=1"}
 , sdpBoardLink = AnchorTag {aText="板",aHref="https://s20.si0.kabu.co.jp/Light/tradetool/reuters/stockboardframe.asp?StockCode=8411&Market=1"}
 , sdpBuyLink = AnchorTag {aText="",aHref="https://s20.si0.kabu.co.jp/ap/Light/Stocks/Stock/Buy/Input?symbol=8411&exchange=1"}
 , sdpSellLink = AnchorTag {aText="",aHref="https://s20.si0.kabu.co.jp/ap/light/Stocks/Stock/Position/SellList?symbol=8411&FilterType=1"}
 , sdpPetitBuyLink = Just AnchorTag {aText="",aHref="https://s20.si0.kabu.co.jp/Light/Trade/PetitBuy/BP01101.asp?StockCode=8411&Market=20"}
 , sdpPetitSellLink = Just AnchorTag {aText="",aHref="https://s20.si0.kabu.co.jp/Light/Trade/PetitSell/SPetitSelect.asp?Brand=8411&FilterType=1&Market=20"}
 , sdpAskPrice = 19970
 , sdpTicker = TSTYO 8411
 , sdpClass = SC貸借銘柄
 , sdpCaption = "みずほフィナンシャルグループ"
 , sdpDay = parseAsiaTokyoDay "2018/02/23"
 , sdpPrice = Just (199.7,HourMinute (15,00))
 , sdpDiff = Just 1.3
 , sdpDiffPc = Just 0.65
 , sdpCloseYesterday = Just 198.4
 , sdpOpen = Just (198.5,HourMinute (09,00))
 , sdpHigh = Just (200.4,HourMinute (14,31))
 , sdpLow = Just (197.7,HourMinute (09,26))
 , sdpOpenAfternoon = Just (199.5, HourMinute (12,30))
 , sdpStdPrice = Just (199.7,DayHourMinute (parseAsiaTokyoDay "2018/02/23",HourMinute (15,00)))
 , sdpDailyHistories =
  [ DailyStockPrice {dspDay=parseAsiaTokyoDay "2018/02/22",dspOpen=Just 198.1,dspHigh=Just 198.7,dspLow=Just 197.2,dspClose=Just 198.4,dspDiff=Just (-1.0),dspVolume=114554500}
  , DailyStockPrice {dspDay=parseAsiaTokyoDay "2018/02/21",dspOpen=Just 201.4,dspHigh=Just 201.5,dspLow=Just 199.1,dspClose=Just 199.4,dspDiff=Just (-2.2),dspVolume=107058000}
  , DailyStockPrice {dspDay=parseAsiaTokyoDay "2018/02/20",dspOpen=Just 201.6,dspHigh=Just 202.1,dspLow=Just 200.0,dspClose=Just 201.6,dspDiff=Just (-1.2),dspVolume=93530900}
  , DailyStockPrice {dspDay=parseAsiaTokyoDay "2018/02/19",dspOpen=Just 200.3,dspHigh=Just 203.1,dspLow=Just 199.9,dspClose=Just 202.8,dspDiff=Just 4.4,dspVolume=126972400}
  , DailyStockPrice {dspDay=parseAsiaTokyoDay "2018/02/16",dspOpen=Just 197.3,dspHigh=Just 199.5,dspLow=Just 197.1,dspClose=Just 198.4,dspDiff=Just 2.1,dspVolume=104328000}
  ]
 }

-- 前場のテストケース
test04StockDetailPage = "https%3A%2F%2Fs10.si0.kabu.co.jp%2FLight%2FTradeTool%2FStockDetail.asp%3FStockCode=8411%26Market=1.utf8.02.html"
test04StockDetailPage' = StockDetailPage
 { sdpChartLink = AnchorTag {aText="チャート",aHref="https://s10.si0.kabu.co.jp/Light/tradetool/reuters/stockchart.asp?chart=4&StockCode=8411&Market=1"}
 , sdpBoardLink = AnchorTag {aText="板",aHref="https://s10.si0.kabu.co.jp/Light/tradetool/reuters/stockboardframe.asp?StockCode=8411&Market=1"}
 , sdpBuyLink = AnchorTag {aText="",aHref="https://s10.si0.kabu.co.jp/ap/Light/Stocks/Stock/Buy/Input?symbol=8411&exchange=1"}
 , sdpSellLink = AnchorTag {aText="",aHref="https://s10.si0.kabu.co.jp/ap/light/Stocks/Stock/Position/SellList?symbol=8411&FilterType=1"}
 , sdpPetitBuyLink = Just AnchorTag {aText="",aHref="https://s10.si0.kabu.co.jp/Light/Trade/PetitBuy/BP01101.asp?StockCode=8411&Market=20"}
 , sdpPetitSellLink = Just AnchorTag {aText="",aHref="https://s10.si0.kabu.co.jp/Light/Trade/PetitSell/SPetitSelect.asp?Brand=8411&FilterType=1&Market=20"}
 , sdpAskPrice = 20260
 , sdpTicker = TSTYO 8411
 , sdpClass = SC貸借銘柄
 , sdpCaption = "みずほフィナンシャルグループ"
 , sdpDay = parseAsiaTokyoDay "2018/02/27"
 , sdpPrice = Just (202.6,HourMinute (09,00))
 , sdpDiff = Just 1.4
 , sdpDiffPc = Just 0.69
 , sdpCloseYesterday = Just 201.2
 , sdpOpen = Just (202.7,HourMinute (09,00))
 , sdpHigh = Just (202.8,HourMinute (09,00))
 , sdpLow = Just (202.6,HourMinute (09,00))
 , sdpOpenAfternoon = Nothing
 , sdpStdPrice = Just (202.6,DayHourMinute (parseAsiaTokyoDay "2018/02/27",HourMinute (09,00)))
 , sdpDailyHistories =
  [ DailyStockPrice {dspDay=parseAsiaTokyoDay "2018/02/26",dspOpen=Just 201.5,dspHigh=Just 202.5,dspLow=Just 200.5,dspClose=Just 201.2,dspDiff=Just 1.5,dspVolume=85358100}
  , DailyStockPrice {dspDay=parseAsiaTokyoDay "2018/02/23",dspOpen=Just 198.5,dspHigh=Just 200.4,dspLow=Just 197.7,dspClose=Just 199.7,dspDiff=Just 1.3,dspVolume=86995000}
  , DailyStockPrice {dspDay=parseAsiaTokyoDay "2018/02/22",dspOpen=Just 198.1,dspHigh=Just 198.7,dspLow=Just 197.2,dspClose=Just 198.4,dspDiff=Just (-1.0),dspVolume=114554500}
  , DailyStockPrice {dspDay=parseAsiaTokyoDay "2018/02/21",dspOpen=Just 201.4,dspHigh=Just 201.5,dspLow=Just 199.1,dspClose=Just 199.4,dspDiff=Just (-2.2),dspVolume=107058000}
  , DailyStockPrice {dspDay=parseAsiaTokyoDay "2018/02/20",dspOpen=Just 201.6,dspHigh=Just 202.1,dspLow=Just 200.0,dspClose=Just 201.6,dspDiff=Just (-1.2),dspVolume=93530900}
  ]
 }

test05StockDetailPage = "https%3A%2F%2Fs10.si0.kabu.co.jp%2FLight%2FTradeTool%2FStockDetail.asp%3FStockCode=6911%26Market=1.utf8.html"
test05StockDetailPage' = StockDetailPage
 { sdpChartLink = AnchorTag {aText="チャート",aHref="https://s10.si0.kabu.co.jp/Light/tradetool/reuters/stockchart.asp?chart=4&StockCode=6911&Market=1"}
 , sdpBoardLink = AnchorTag {aText="板",aHref="https://s10.si0.kabu.co.jp/Light/tradetool/reuters/stockboardframe.asp?StockCode=6911&Market=1"}
 , sdpBuyLink = AnchorTag {aText="",aHref="https://s10.si0.kabu.co.jp/ap/Light/Stocks/Stock/Buy/Input?symbol=6911&exchange=1"}
 , sdpSellLink = AnchorTag {aText="",aHref="https://s10.si0.kabu.co.jp/ap/light/Stocks/Stock/Position/SellList?symbol=6911&FilterType=1"}
 , sdpPetitBuyLink = Just AnchorTag {aText="",aHref="https://s10.si0.kabu.co.jp/Light/Trade/PetitBuy/BP01101.asp?StockCode=6911&Market=20"}
 , sdpPetitSellLink = Just AnchorTag {aText="",aHref="https://s10.si0.kabu.co.jp/Light/Trade/PetitSell/SPetitSelect.asp?Brand=6911&FilterType=1&Market=20"}
 , sdpAskPrice = 90400
 , sdpTicker = TSTYO 6911
 , sdpClass = SC貸借銘柄
 , sdpCaption = "新日本無線"
 , sdpDay = parseAsiaTokyoDay "2018/02/28"
 , sdpPrice = Nothing
 , sdpDiff = Nothing
 , sdpDiffPc = Nothing
 , sdpCloseYesterday = Just 904
 , sdpOpen = Nothing
 , sdpHigh = Nothing
 , sdpLow = Nothing
 , sdpOpenAfternoon = Nothing
 , sdpStdPrice = Just (904,DayHourMinute (parseAsiaTokyoDay "2018/02/27",HourMinute (00,00)))
 , sdpDailyHistories =
  [ DailyStockPrice {dspDay=parseAsiaTokyoDay "2018/02/27",dspOpen=Just 885,dspHigh=Just 914,dspLow=Just 885,dspClose=Just 904,dspDiff=Just 18,dspVolume=152300}
  , DailyStockPrice {dspDay=parseAsiaTokyoDay "2018/02/26",dspOpen=Just 891,dspHigh=Just 902,dspLow=Just 877,dspClose=Just 886,dspDiff=Just (-4),dspVolume=62700}
  , DailyStockPrice {dspDay=parseAsiaTokyoDay "2018/02/23",dspOpen=Just 880,dspHigh=Just 894,dspLow=Just 877,dspClose=Just 890,dspDiff=Just 12,dspVolume=111400}
  , DailyStockPrice {dspDay=parseAsiaTokyoDay "2018/02/22",dspOpen=Just 900,dspHigh=Just 900,dspLow=Just 869,dspClose=Just 878,dspDiff=Just 0,dspVolume=137800}
  , DailyStockPrice {dspDay=parseAsiaTokyoDay "2018/02/21",dspOpen=Just 870,dspHigh=Just 893,dspLow=Just 868,dspClose=Just 878,dspDiff=Just 4,dspVolume=106500}
  ]
 }

test06StockDetailPage = "https%3A%2F%2Fs10.kabu.co.jp%2FLight%2FTradeTool%2FStockDetail.asp%3FStockCode=3788%26Market=1.utf8.html"
test06StockDetailPage' = StockDetailPage
 { sdpChartLink = AnchorTag {aText="チャート",aHref="https://s10.si0.kabu.co.jp/Light/tradetool/reuters/stockchart.asp?chart=4&StockCode=3788&Market=1"}
 , sdpBoardLink = AnchorTag {aText="板",aHref="https://s10.si0.kabu.co.jp/Light/tradetool/reuters/stockboardframe.asp?StockCode=3788&Market=1"}
 , sdpBuyLink = AnchorTag {aText="",aHref="https://s10.si0.kabu.co.jp/ap/Light/Stocks/Stock/Buy/Input?symbol=3788&exchange=1"}
 , sdpSellLink = AnchorTag {aText="",aHref="https://s10.si0.kabu.co.jp/ap/light/Stocks/Stock/Position/SellList?symbol=3788&FilterType=1"}
 , sdpPetitBuyLink = Just AnchorTag {aText="",aHref="https://s10.si0.kabu.co.jp/Light/Trade/PetitBuy/BP01101.asp?StockCode=3788&Market=20"}
 , sdpPetitSellLink = Just AnchorTag {aText="",aHref="https://s10.si0.kabu.co.jp/Light/Trade/PetitSell/SPetitSelect.asp?Brand=3788&FilterType=1&Market=20"}
 , sdpAskPrice = 237000
 , sdpTicker = TSTYO 3788
 , sdpClass = SC貸借銘柄
 , sdpCaption = "ＧＭＯクラウド"
 , sdpDay = parseAsiaTokyoDay "2018/03/02"
 , sdpPrice = Just (2370,HourMinute (15,00))
 , sdpDiff = Just (-15)
 , sdpDiffPc = Just (-0.62)
 , sdpCloseYesterday = Just 2385
 , sdpOpen = Just (2335,HourMinute (09,00))
 , sdpHigh = Just (2386,HourMinute (12,58))
 , sdpLow = Just (2333,HourMinute (09,10))
 , sdpOpenAfternoon = Just (2336,HourMinute (12,30))
 , sdpStdPrice = Just (2370,DayHourMinute (parseAsiaTokyoDay "2018/03/02",HourMinute (15,00)))
 , sdpDailyHistories =
  [ DailyStockPrice {dspDay=parseAsiaTokyoDay "2018/03/01",dspOpen=Just 2400,dspHigh=Just 2406,dspLow=Just 2335,dspClose=Just 2385,dspDiff=Just 9,dspVolume=57700}
  , DailyStockPrice {dspDay=parseAsiaTokyoDay "2018/02/28",dspOpen=Just 2389,dspHigh=Just 2470,dspLow=Just 2374,dspClose=Just 2376,dspDiff=Just (-35),dspVolume=71500}
  , DailyStockPrice {dspDay=parseAsiaTokyoDay "2018/02/27",dspOpen=Just 2460,dspHigh=Just 2467,dspLow=Just 2360,dspClose=Just 2411,dspDiff=Just (-31),dspVolume=56300}
  , DailyStockPrice {dspDay=parseAsiaTokyoDay "2018/02/26",dspOpen=Just 2435,dspHigh=Just 2473,dspLow=Just 2421,dspClose=Just 2442,dspDiff=Just 31,dspVolume=81000}
  , DailyStockPrice {dspDay=parseAsiaTokyoDay "2018/02/23",dspOpen=Just 2341,dspHigh=Just 2447,dspLow=Just 2327,dspClose=Just 2411,dspDiff=Just 81,dspVolume=114600}
  ]
 }

test07StockDetailPage = "https%3A%2F%2Fs10.kabu.co.jp%2FLight%2FTradeTool%2FStockDetail.asp%3FStockCode=6770%26Market=1.utf8.html"
test07StockDetailPage' = StockDetailPage
 { sdpChartLink = AnchorTag {aText="チャート",aHref="https://s10.si0.kabu.co.jp/Light/tradetool/reuters/stockchart.asp?chart=4&StockCode=6770&Market=1"}
 , sdpBoardLink = AnchorTag {aText="板",aHref="https://s10.si0.kabu.co.jp/Light/tradetool/reuters/stockboardframe.asp?StockCode=6770&Market=1"}
 , sdpBuyLink = AnchorTag {aText="",aHref="https://s10.si0.kabu.co.jp/ap/Light/Stocks/Stock/Buy/Input?symbol=6770&exchange=1"}
 , sdpSellLink = AnchorTag {aText="",aHref="https://s10.si0.kabu.co.jp/ap/light/Stocks/Stock/Position/SellList?symbol=6770&FilterType=1"}
 , sdpPetitBuyLink = Just AnchorTag {aText="",aHref="https://s10.si0.kabu.co.jp/Light/Trade/PetitBuy/BP01101.asp?StockCode=6770&Market=20"}
 , sdpPetitSellLink = Just AnchorTag {aText="",aHref="https://s10.si0.kabu.co.jp/Light/Trade/PetitSell/SPetitSelect.asp?Brand=6770&FilterType=1&Market=20"}
 , sdpAskPrice = 278700
 , sdpTicker = TSTYO 6770
 , sdpClass = SC貸借銘柄
 , sdpCaption = "アルプス電気"
 , sdpDay = parseAsiaTokyoDay "2018/03/14"
 , sdpPrice = Just (2787,HourMinute (15,00))
 , sdpDiff = Just (-18)
 , sdpDiffPc = Just (-0.64)
 , sdpCloseYesterday = Just 2805
 , sdpOpen = Just (2780,HourMinute (09,00))
 , sdpHigh = Just (2815,HourMinute (09,49))
 , sdpLow = Just (2756,HourMinute (09,03))
 , sdpOpenAfternoon = Just (2791,HourMinute (12,30))
 , sdpStdPrice = Just (2787,DayHourMinute (parseAsiaTokyoDay "2018/03/14",HourMinute (15,00)))
 , sdpDailyHistories =
  [ DailyStockPrice {dspDay=parseAsiaTokyoDay "2018/03/13",dspOpen=Just 2748,dspHigh=Just 2810,dspLow=Just 2741,dspClose=Just 2805,dspDiff=Just 52,dspVolume=2941200}
  , DailyStockPrice {dspDay=parseAsiaTokyoDay "2018/03/12",dspOpen=Just 2759,dspHigh=Just 2778,dspLow=Just 2729,dspClose=Just 2753,dspDiff=Just 52,dspVolume=3117500}
  , DailyStockPrice {dspDay=parseAsiaTokyoDay "2018/03/09",dspOpen=Just 2719,dspHigh=Just 2760,dspLow=Just 2685,dspClose=Just 2701,dspDiff=Just (-10),dspVolume=4385700}
  , DailyStockPrice {dspDay=parseAsiaTokyoDay "2018/03/08",dspOpen=Just 2737,dspHigh=Just 2739,dspLow=Just 2678,dspClose=Just 2711,dspDiff=Just 0,dspVolume=4255700}
  , DailyStockPrice {dspDay=parseAsiaTokyoDay "2018/03/07",dspOpen=Just 2713,dspHigh=Just 2768,dspLow=Just 2703,dspClose=Just 2711,dspDiff=Just (-30),dspVolume=2702500}
  ]
 }

test08StockDetailPage = "https%3A%2F%2Fs10.kabu.co.jp%2FLight%2FTradeTool%2FStockDetail.asp%3FStockCode=3038%26Market=1.utf8.html"
test08StockDetailPage' = StockDetailPage
 { sdpChartLink = AnchorTag {aText="チャート",aHref="https://s10.si0.kabu.co.jp/Light/tradetool/reuters/stockchart.asp?chart=4&StockCode=3038&Market=1"}
 , sdpBoardLink = AnchorTag {aText="板",aHref="https://s10.si0.kabu.co.jp/Light/tradetool/reuters/stockboardframe.asp?StockCode=3038&Market=1"}
 , sdpBuyLink = AnchorTag {aText="",aHref="https://s10.si0.kabu.co.jp/ap/Light/Stocks/Stock/Buy/Input?symbol=3038&exchange=1"}
 , sdpSellLink = AnchorTag {aText="",aHref="https://s10.si0.kabu.co.jp/ap/light/Stocks/Stock/Position/SellList?symbol=3038&FilterType=1"}
 , sdpPetitBuyLink = Just AnchorTag {aText="",aHref="https://s10.si0.kabu.co.jp/Light/Trade/PetitBuy/BP01101.asp?StockCode=3038&Market=20"}
 , sdpPetitSellLink = Just AnchorTag {aText="",aHref="https://s10.si0.kabu.co.jp/Light/Trade/PetitSell/SPetitSelect.asp?Brand=3038&FilterType=1&Market=20"}
 , sdpAskPrice = 433000
 , sdpTicker = TSTYO 3038
 , sdpClass = SC融資銘柄
 , sdpCaption = "神戸物産"
 , sdpDay = parseAsiaTokyoDay "2018/03/14"
 , sdpPrice = Just (4330,HourMinute (15,00))
 , sdpDiff = Just (-45)
 , sdpDiffPc = Just (-1.02)
 , sdpCloseYesterday = Just 4375
 , sdpOpen = Just (4360,HourMinute (09,00))
 , sdpHigh = Just (4440,HourMinute (09,14))
 , sdpLow = Just (4315,HourMinute (14,29))
 , sdpOpenAfternoon = Just (4350,HourMinute (12,30))
 , sdpStdPrice = Just (4330,DayHourMinute (parseAsiaTokyoDay "2018/03/14",HourMinute (15,00)))
 , sdpDailyHistories =
  [ DailyStockPrice {dspDay=parseAsiaTokyoDay "2018/03/13",dspOpen=Just 4295,dspHigh=Just 4390,dspLow=Just 4295,dspClose=Just 4375,dspDiff=Just 45,dspVolume=141800}
  , DailyStockPrice {dspDay=parseAsiaTokyoDay "2018/03/12",dspOpen=Just 4360,dspHigh=Just 4370,dspLow=Just 4295,dspClose=Just 4330,dspDiff=Just (-20),dspVolume=112300}
  , DailyStockPrice {dspDay=parseAsiaTokyoDay "2018/03/09",dspOpen=Just 4370,dspHigh=Just 4415,dspLow=Just 4310,dspClose=Just 4350,dspDiff=Just 20,dspVolume=210600}
  , DailyStockPrice {dspDay=parseAsiaTokyoDay "2018/03/08",dspOpen=Just 4320,dspHigh=Just 4340,dspLow=Just 4260,dspClose=Just 4330,dspDiff=Just 20,dspVolume=309100}
  , DailyStockPrice {dspDay=parseAsiaTokyoDay "2018/03/07",dspOpen=Just 4290,dspHigh=Just 4375,dspLow=Just 4290,dspClose=Just 4310,dspDiff=Just (-20),dspVolume=125000}
  ]
 }

parseAsiaTokyoDay :: String -> AsiaTokyoDay
parseAsiaTokyoDay =
    AsiaTokyoDay . Tm.parseTimeOrError True Tm.defaultTimeLocale "%Y/%m/%d"

--
-- Hspecテスト
--
spec :: Spec
spec = do
    --
    describe "formLoginPage" $
        it "https://s10.kabu.co.jp/_mem_bin/light/login.asp?/light" $ do
            html <- TL.readFile ("test/KabuCom/" ++ test01LoginPage)
            formLoginPage html `shouldReturn` test01LoginPage'
    --
    describe "topPage" $
        it "https://s20.si0.kabu.co.jp/Light/" $ do
            html <- TL.readFile ("test/KabuCom/" ++ test01TopPage)
            topPage html `shouldBe` test01TopPage'
    --
    describe "purchaseMarginPage" $
        it "https://s20.si0.kabu.co.jp/ap/light/Assets/Kanougaku/Stock" $ do
            html <- TL.readFile ("test/KabuCom/" ++ test01PurchaseMarginPage)
            purchaseMarginPage html `shouldReturn` test01PurchaseMarginPage'
    --
    describe "stockPositionListPage" $
        it "https://s20.si0.kabu.co.jp/ap/light/Stocks/Stock/Position/List" $ do
            html <- TL.readFile ("test/KabuCom/" ++ test01StockPositionListPage)
            stockPositionListPage html `shouldReturn` test01StockPositionListPage'
    --
    describe "stockDetailPage" $ do
        it "https://s20.si0.kabu.co.jp/Light/TradeTool/StockDetail.asp?StockCode=1343&Market=1" $ do
            html <- TL.readFile ("test/KabuCom/" ++ test01StockDetailPage)
            stockDetailPage html `shouldReturn` test01StockDetailPage'
        --
        it "https://s20.si0.kabu.co.jp/Light/TradeTool/StockDetail.asp?StockCode=1540&Market=1" $ do
            html <- TL.readFile ("test/KabuCom/" ++ test02StockDetailPage)
            stockDetailPage html `shouldReturn` test02StockDetailPage'
        --
        it "https://s20.si0.kabu.co.jp/Light/TradeTool/StockDetail.asp?StockCode=8411&Market=1 01" $ do
            html <- TL.readFile ("test/KabuCom/" ++ test03StockDetailPage)
            stockDetailPage html `shouldReturn` test03StockDetailPage'
        --
        it "https://s10.si0.kabu.co.jp/Light/TradeTool/StockDetail.asp?StockCode=8411&Market=1 02" $ do
            html <- TL.readFile ("test/KabuCom/" ++ test04StockDetailPage)
            stockDetailPage html `shouldReturn` test04StockDetailPage'
        --
        it "https://s10.si0.kabu.co.jp/Light/TradeTool/StockDetail.asp?StockCode=6911&Market=1" $ do
            html <- TL.readFile ("test/KabuCom/" ++ test05StockDetailPage)
            stockDetailPage html `shouldReturn` test05StockDetailPage'
        --
        it "https://s10.kabu.co.jp/Light/TradeTool/StockDetail.asp?StockCode=3788&Market=1" $ do
            html <- TL.readFile ("test/KabuCom/" ++ test06StockDetailPage)
            stockDetailPage html `shouldReturn` test06StockDetailPage'
        --
        it "https://s10.kabu.co.jp/Light/TradeTool/StockDetail.asp?StockCode=6770&Market=1" $ do
            html <- TL.readFile ("test/KabuCom/" ++ test07StockDetailPage)
            stockDetailPage html `shouldReturn` test07StockDetailPage'
        --
        it "https://s10.kabu.co.jp/Light/TradeTool/StockDetail.asp?StockCode=3038&Market=1" $ do
            html <- TL.readFile ("test/KabuCom/" ++ test08StockDetailPage)
            stockDetailPage html `shouldReturn` test08StockDetailPage'

