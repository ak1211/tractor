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
   Module      :  Msg
   Description :  This file is web front main module of Application "Tractor"
   Copyright   :  (c) 2016 Akihiro Yamamoto
   License     :  AGPLv3

   Maintainer  :  https://github.com/ak1211
   Stability   :  unstable
   Portability :  POSIX

   アプリケーション「Tractor」のフロント側メッセージ定義です。
-}


module Msg exposing (Msg(..))

import Generated.WebApi as WebApi
import Http
import Material
import Navigation
import PortfolioPage.Msg as PortfolioPage
import UploadPage.Msg as UploadPage
import AnalyticsPage.Msg as AnalyticsPage


type Msg
    = DoneOAuthExchangeCode (Result Http.Error WebApi.BearerToken)
    | NewUrl String
    | Nop
    | ScrollToTop
    | UpdateServerVersion (Result Http.Error WebApi.VerRev)
    | UpdateWebApiDocument (Result Http.Error String)
    | UrlChange Navigation.Location
    | UploadPageMsg UploadPage.Msg
    | PortfolioPageMsg PortfolioPage.Msg
    | AnalyticsPageMsg AnalyticsPage.Msg
    | Mdl (Material.Msg Msg)
