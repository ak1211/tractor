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
   Module      :  AnalyticsPage.Msg
   Description :  This file is web front main module of Application "Tractor"
   Copyright   :  (c) 2016 Akihiro Yamamoto
   License     :  AGPLv3

   Maintainer  :  https://github.com/ak1211
   Stability   :  unstable
   Portability :  POSIX

   Analyticsページのメッセージ定義です。
-}


module AnalyticsPage.Msg exposing (..)

import Generated.WebApi as WebApi
import Http
import Material


type Msg
    = ChangeAccessToken (Maybe WebApi.AccessToken)
    | ChangeMarketCode (Maybe WebApi.MarketCode)
    | UpdateHistories WebApi.MarketCode (Result Http.Error (List WebApi.ApiOhlcv))
    | SelectTab Int
    | Mdl (Material.Msg Msg)
