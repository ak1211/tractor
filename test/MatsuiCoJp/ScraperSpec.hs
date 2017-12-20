{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
module MatsuiCoJp.ScraperSpec (spec) where

import qualified Data.Text.Lazy     as TL
import qualified Data.Text.Lazy.IO  as TL
import           MatsuiCoJp.Scraper
import           Test.Hspec

--
-- テストファイル
--
test01HomeAnnounce = "01www.deal.matsui.co.jp_servlet_ITS_home_Announce.utf8.html"
test01HomeAnnounce' =
    FraHomeAnnounce
        { fsAnnounceDeriverTime     = "2017年12月18日 23:25 現在"
        , fsAnnounceLastLoginTime   = "前回ログイン：2017年12月18日 23:25"
        , fsAnnounces               =   [ "****　****様 へのご連絡"
                                        , "本日の注文 4件、約定 1件"
                                        , "その他お客様宛のメッセージはありません。"
                                        , "会員様へのご連絡"
                                        , "【投資信託】「コモンズ30ファンド」や「AI(人工知能)活用型世界株ファンド」等24銘柄の取扱いを開始します"
                                        , "立会外分売受付中（～12/19 08:30まで）"
                                        , "【投資信託】投信工房に自動リバランス機能等を追加します（12/23～）"
                                        , "「週刊　株式相場レポート」を更新しました（12/15）"
                                        , "【一日信用取引】プレミアム空売り銘柄を追加します（12/18取引分～）"
                                        , "【重要】12月末決算・分割銘柄を保有（取引）するお客様へ"
                                        , "【一日信用取引】プレミアム空売り銘柄を追加します（12/15取引分～）"
                                        , "【一日信用取引】プレミアム空売り銘柄の建玉上限変更について"
                                        , "年末年始の営業時間のお知らせ（2017-2018）"
                                        , "12月の株主優待銘柄のご案内！"
                                        , "10万円超のお取引であたる優待応援カタログギフトキャンペーン"
                                        , "【好評につき期間延長】一日信用取引　金利引下げキャンペーン"
                                        , "【期間延長】『iシェアーズETF』買付手数料実質無料キャンペーン"
                                        , "【ご案内】つみたてNISAの申込を受付中です"
                                        , "ご家族・ご友人紹介プログラムを実施中です"
                                        , " "
                                        , "株式銘柄情報"
                                        , "メッセージ7件"
                                        ]
        }

test01StkHavingList = "01www.deal.matsui.co.jp_servlet_ITS_stock_StkHavingList.utf8.html"
test01StkSell =
    FraStkSell
        { fsQuantity    = 561.0
        , fsProfit      = -6.0
        , fsStocks      = [ HoldStock
                            { hsSellOrderUrl  = Just "/servlet/ITS/stock/StkSellOrder;"
                            , hsCode          = 9399
                            , hsCaption       = "新華ホールディングス・リミテッド"
                            , hsCount         = 3
                            , hsPurchasePrice = 189.0
                            , hsPrice         = 187.0
                            } ]
        }

test02StkHavingList = "02www.deal.matsui.co.jp_servlet_ITS_stock_StkHavingList.utf8.html"
test02StkSell =
    FraStkSell
        { fsQuantity    = 561.0
        , fsProfit      = -6.0
        , fsStocks      = [ HoldStock
                            { hsSellOrderUrl  = Just "/servlet/ITS/stock/StkSellOrder;"
                            , hsCode          = 9399
                            , hsCaption       = "新華ホールディングス・リミテッド"
                            , hsCount         = 3
                            , hsPurchasePrice = 189.0
                            , hsPrice         = 187.0
                            } ]
        }

test01AstSpare = "01www.deal.matsui.co.jp_servlet_ITS_asset_MoneyToSpare.utf8.html"
test01AstSpare' =
    FraAstSpare
        { faMoneyToSpare        = 402
        , faStockOfMoney        = 1482
        , faIncreaseOfDeposits  = 0
        , faDecreaseOfDeposits  = 0
        , faRestraintFee        = 0
        , faRestraintTax        = 0
        , faCash                = 1482
        }

--
-- Hspecテスト
--
spec :: Spec
spec = do
    --
    describe "scrapingFraHomeAnnounce" $ do
        it "test 01" $ do
            html <- TL.readFile ("test/MatsuiCoJp/" ++ test01HomeAnnounce)
            case scrapingFraHomeAnnounce [html] of
                Left l -> TL.putStrLn l
                Right r -> r `shouldBe` test01HomeAnnounce'
    --
    describe "scrapingFraStkSell" $ do
        it "test 01" $ do
            html <- TL.readFile ("test/MatsuiCoJp/" ++ test01StkHavingList)
            case scrapingFraStkSell [html] of
                Left l -> TL.putStrLn l
                Right r -> r `shouldBe` test01StkSell
        --
--        it "test 02" $ do
--            html <- TL.readFile ("test/MatsuiCoJp/" ++ test02StkHavingList)
--            scrapingFraStkSell [html] `shouldBe` (Right test02StkSell)
    --
    describe "scrapingFraAstSpare" $ do
        it "test 01" $ do
            html <- TL.readFile ("test/MatsuiCoJp/" ++ test01AstSpare)
            case scrapingFraAstSpare [html] of
                Left l -> TL.putStrLn l
                Right r -> r `shouldBe` test01AstSpare'

