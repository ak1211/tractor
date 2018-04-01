{-
    This file is part of Tractor.

    Tractor is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Tractor is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with Tractor.  If not, see <http://www.gnu.org/licenses/>.
-}
{- |
Module      :  BackOffice.Agency
Description :
Copyright   :  (c) 2016 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  POSIX

-}
{-# LANGUAGE StrictData        #-}
module BackOffice.Agency
    where
import BackOffice.StockQuotesCrawler
import qualified Conf

-- |
-- 情報更新
updateSomeRates :: Conf.Info -> IO ()
updateSomeRates conf =
    webCrawling
    where
    -- |
    -- Webクローリング
    webCrawling = runWebCrawlingPortfolios conf
    -- |
    -- 集計
--    aggregate = Aggregate.runAggregateOfPortfolios conf

