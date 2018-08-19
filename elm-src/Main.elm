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
   Module      :  Main
   Description :  This file is web front main module of Application "Tractor"
   Copyright   :  (c) 2016 Akihiro Yamamoto
   License     :  AGPLv3

   Maintainer  :  https://github.com/ak1211
   Stability   :  unstable
   Portability :  POSIX

   アプリケーション「Tractor」のフロント側メインモジュールです。
-}


module Main exposing (main)

import AnalyticsPage.Model as AnalyticsPage
import Generated.WebApi as WebApi
import Generated.WebApi exposing (ApiOhlcv)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import Material
import Maybe
import Model exposing (Model)
import Msg exposing (Msg)
import Navigation
import PortfolioPage.Model as PortfolioPage
import Route exposing (QueryCode, Route)
import UploadPage.Model as UploadPage
import Update
import View


exchangeOAuthCodeForToken : String -> Cmd Msg
exchangeOAuthCodeForToken code =
    Http.send Msg.DoneOAuthExchangeCode (WebApi.postApiV1Auth code)


getWebApiDocument : Cmd Msg
getWebApiDocument =
    Http.send Msg.UpdateWebApiDocument (Http.getString "public/WebApiDocument.md")


getServerVersion : Cmd Msg
getServerVersion =
    Http.send Msg.UpdateServerVersion WebApi.getApiV1Version


subscriptions : Model -> Sub Msg
subscriptions model =
    Material.subscriptions Msg.Mdl model


type alias Flags =
    { client_id : String
    }


decodeFlags : Decode.Decoder Flags
decodeFlags =
    Decode.decode Flags
        |> Decode.required "client_id" Decode.string


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
    let
        ( analyticsPageModel, analyticsPageCmd ) =
            AnalyticsPage.init

        ( portfolioPageModel, portfolioPageCmd ) =
            PortfolioPage.init

        ( uploadPageModel, uploadPageCmd ) =
            UploadPage.init

        initialModel =
            { clientID = flags.client_id
            , accessToken = Nothing
            , userName = Nothing
            , pageHistory = []
            , serverVersion = Nothing
            , webApiDocument = Nothing
            , uploadPageModel = uploadPageModel
            , portfolioPageModel = portfolioPageModel
            , analyticsPageModel = analyticsPageModel
            , mdl = Material.model
            }

        model =
            case Route.fromLocation location of
                Just a ->
                    { initialModel | pageHistory = [ a ] }

                Nothing ->
                    initialModel

        exchangeCode =
            case Route.fromLocation location of
                Just (Route.Home Nothing (Just code) _) ->
                    exchangeOAuthCodeForToken code

                _ ->
                    Cmd.none
    in
        model
            ! [ Material.init Msg.Mdl
              , Cmd.map Msg.AnalyticsPageMsg analyticsPageCmd
              , Cmd.map Msg.PortfolioPageMsg portfolioPageCmd
              , Cmd.map Msg.UploadPageMsg uploadPageCmd
              , getServerVersion
              , getWebApiDocument
              , exchangeCode
              ]


main : Program Flags Model Msg
main =
    Navigation.programWithFlags Msg.UrlChange
        { init = init
        , view = View.view
        , subscriptions = subscriptions
        , update = Update.update
        }
