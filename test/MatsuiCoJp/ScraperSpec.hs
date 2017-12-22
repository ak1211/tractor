{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
module MatsuiCoJp.ScraperSpec (spec) where

import qualified Data.Text.Lazy     as TL
import qualified Data.Text.Lazy.IO  as TL
import           MatsuiCoJp.Scraper
import           Test.Hspec

--
-- "ホーム" -> "お知らせ" のページ
--
test01HomeAnnounce = "01www.deal.matsui.co.jp_servlet_ITS_home_Announce.utf8.html"
test01HomeAnnounce' = FraHomeAnnounce
    { announceDeriverTime   = "2017年12月18日 23:25 現在"
    , announceLastLoginTime = "前回ログイン：2017年12月18日 23:25"
    , announces             =   [ "****　****様 へのご連絡"
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

--
--  "株式取引" -> "現物売" のページ
--
-- 保有株式1つの場合
test01StkHavingList = "01www.deal.matsui.co.jp_servlet_ITS_stock_StkHavingList.utf8.html"
test01StkSell = FraStkSell
    { evaluation    = 561.0
    , profit        = -6.0
    , stocks        = [ HoldStock
                        { sellOrderUrl  = Just "/servlet/ITS/stock/StkSellOrder;"
                        , code          = 9399
                        , caption       = "新華ホールディングス・リミテッド"
                        , count         = 3
                        , purchasePrice = 189.0
                        , price         = 187.0
                        } ]
    }

-- 保有株式なしの場合
test02StkHavingList = "02www.deal.matsui.co.jp_servlet_ITS_stock_StkHavingList.utf8.html"
test02StkSell = FraStkSell
    { evaluation    = 0.0
    , profit        = 0.0
    , stocks        = []
    }

--
-- 資産状況 -> 余力情報のページ
--
test01AstSpare = "01www.deal.matsui.co.jp_servlet_ITS_asset_MoneyToSpare.utf8.html"
test01AstSpare' = FraAstSpare
    { moneySpare    = 402
    , cashBalance   = 1482
    , depositInc    = 0
    , depositDec    = 0
    , bindingFee    = 0
    , bindingTax    = 0
    , freeCash      = 1482
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
        it "test 02" $ do
            html <- TL.readFile ("test/MatsuiCoJp/" ++ test02StkHavingList)
            scrapingFraStkSell [html] `shouldBe` (Right test02StkSell)
    --
    describe "scrapingFraAstSpare" $ do
        it "test 01" $ do
            html <- TL.readFile ("test/MatsuiCoJp/" ++ test01AstSpare)
            case scrapingFraAstSpare [html] of
                Left l -> TL.putStrLn l
                Right r -> r `shouldBe` test01AstSpare'

