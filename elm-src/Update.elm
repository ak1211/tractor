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
   Module      :  Update
   Description :  This file is web front main module of Application "Tractor"
   Copyright   :  (c) 2016 Akihiro Yamamoto
   License     :  AGPLv3

   Maintainer  :  https://github.com/ak1211
   Stability   :  unstable
   Portability :  POSIX

   アプリケーション「Tractor」のアップデートモジュールです。
-}


module Update exposing (update)

import AnalyticsPage.Msg as AnalyticsPage
import AnalyticsPage.Update as AnalyticsPage
import Dom.Scroll
import Material
import Material.Layout as Layout
import Maybe
import Model exposing (Model)
import Msg exposing (Msg)
import Navigation
import PortfolioPage.Msg as PortfolioPage
import PortfolioPage.Update as PortfolioPage
import Route exposing (QueryCode, Route)
import Task
import UploadPage.Msg as UploadPage
import UploadPage.Update as UploadPage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg.DoneOAuthExchangeCode result ->
            let
                ( newModel, token ) =
                    case result of
                        Ok ok ->
                            let
                                m =
                                    { model
                                        | accessToken = Just ok.accessToken
                                        , userName = Just ok.userName
                                    }
                            in
                                ( m, Just ok.accessToken )

                        Err _ ->
                            let
                                m =
                                    { model
                                        | accessToken = Nothing
                                        , userName = Nothing
                                    }
                            in
                                ( m, Nothing )

                msgs =
                    [ Msg.UploadPageMsg (UploadPage.ChangeAccessToken token)
                    , Msg.PortfolioPageMsg (PortfolioPage.ChangeAccessToken token)
                    , Msg.AnalyticsPageMsg (AnalyticsPage.ChangeAccessToken token)
                    ]
            in
                newModel ! List.map (Task.perform identity << Task.succeed) msgs

        Msg.UploadPageMsg subMsg ->
            let
                ( newModel, newCmd ) =
                    UploadPage.update subMsg model.uploadPageModel
            in
                { model | uploadPageModel = newModel } ! [ Cmd.map Msg.UploadPageMsg newCmd ]

        Msg.PortfolioPageMsg subMsg ->
            let
                ( newModel, newCmd ) =
                    PortfolioPage.update subMsg model.portfolioPageModel
            in
                { model | portfolioPageModel = newModel } ! [ Cmd.map Msg.PortfolioPageMsg newCmd ]

        Msg.AnalyticsPageMsg subMsg ->
            let
                ( newModel, newCmd ) =
                    AnalyticsPage.update subMsg model.analyticsPageModel
            in
                { model | analyticsPageModel = newModel } ! [ Cmd.map Msg.AnalyticsPageMsg newCmd ]

        Msg.Mdl subMsg ->
            Material.update Msg.Mdl subMsg model

        Msg.NewUrl url ->
            model ! [ Navigation.newUrl url ]

        Msg.Nop ->
            ( model, Cmd.none )

        Msg.ScrollToTop ->
            model ! [ Task.attempt (always Msg.Nop) <| Dom.Scroll.toTop Layout.mainId ]

        Msg.UpdateServerVersion res ->
            { model | serverVersion = Result.toMaybe res } ! []

        Msg.UpdateWebApiDocument res ->
            { model | webApiDocument = Result.toMaybe res } ! []

        Msg.UrlChange location ->
            urlUpdate model location


urlUpdate : Model -> Navigation.Location -> ( Model, Cmd Msg )
urlUpdate model location =
    case Route.fromLocation location of
        Just next ->
            { model | pageHistory = next :: model.pageHistory }
                ! [ Task.attempt (always Msg.Nop) <| Dom.Scroll.toTop Layout.mainId
                  , Task.perform identity << Task.succeed <| Msg.UploadPageMsg (UploadPage.UrlChange next)
                  , Task.perform identity << Task.succeed <| Msg.PortfolioPageMsg (PortfolioPage.UrlChange next)
                  , Task.perform identity << Task.succeed <| Msg.AnalyticsPageMsg (AnalyticsPage.UrlChange next)
                  ]

        Nothing ->
            model ! []