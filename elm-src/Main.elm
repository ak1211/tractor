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


module Main exposing (..)

import Dom.Scroll
import Generated.WebApi as WebApi
import Generated.WebApi exposing (ApiOhlcv)
import Http
import Material
import Material.Layout as Layout
import Maybe
import Model exposing (Model)
import Msg exposing (Msg)
import Navigation
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import UploadPage.Update as UploadPage
import UploadPage.Msg as UploadPage
import Route exposing (QueryCode, Route)
import Task
import View


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg.DoneOAuthExchangeCode res ->
            let
                ( newModel, newCmd ) =
                    UploadPage.update
                        (UploadPage.DoneOAuthExchangeCode res)
                        model.uploadPageModel
            in
                case res of
                    Ok ok ->
                        { model
                            | accessToken = Just ok.accessToken
                            , userName = Just ok.userName
                            , uploadPageModel = newModel
                        }
                            ! [ getPortfolios ok.accessToken
                              , Cmd.map Msg.UploadPageMsg newCmd
                              ]

                    Err _ ->
                        { model
                            | accessToken = Nothing
                            , userName = Nothing
                            , uploadPageModel = newModel
                        }
                            ! [ Cmd.map Msg.UploadPageMsg newCmd ]

        Msg.UploadPageMsg subMsg ->
            let
                ( newModel, newCmd ) =
                    UploadPage.update subMsg model.uploadPageModel
            in
                ( { model | uploadPageModel = newModel }
                , Cmd.map Msg.UploadPageMsg newCmd
                )

        Msg.Mdl subMsg ->
            Material.update Msg.Mdl subMsg model

        Msg.NewUrl url ->
            model ! [ Navigation.newUrl url ]

        Msg.Nop ->
            ( model, Cmd.none )

        Msg.ScrollToTop ->
            model ! [ Task.attempt (always Msg.Nop) <| Dom.Scroll.toTop Layout.mainId ]

        Msg.UpdateAnalytics res ->
            { model | histories = Result.toMaybe res } ! []

        Msg.UpdatePortfolios res ->
            { model | portfolios = Result.toMaybe res } ! []

        Msg.UpdateServerVersion res ->
            { model | serverVersion = Result.toMaybe res } ! []

        Msg.UpdateWebApiDocument res ->
            { model | webApiDocument = Result.toMaybe res } ! []

        Msg.UrlChange location ->
            urlUpdate model location


urlUpdate : Model -> Navigation.Location -> ( Model, Cmd Msg )
urlUpdate model location =
    case Route.fromLocation location of
        Just (Route.Analytics (Just ts)) ->
            let
                next =
                    Route.Analytics (Just ts)

                history =
                    case model.accessToken of
                        Just token ->
                            getHistories token ts

                        Nothing ->
                            Cmd.none
            in
                { model | pageHistory = next :: model.pageHistory }
                    ! [ history ]

        Just next ->
            { model | pageHistory = next :: model.pageHistory }
                ! [ Task.attempt (always Msg.Nop) <| Dom.Scroll.toTop Layout.mainId ]

        Nothing ->
            model ! []


getWebApiDocument : Cmd Msg
getWebApiDocument =
    Http.send Msg.UpdateWebApiDocument (Http.getString "public/WebApiDocument.md")


exchangeOAuthCodeForToken : String -> Cmd Msg
exchangeOAuthCodeForToken code =
    Http.send Msg.DoneOAuthExchangeCode (WebApi.getApiV1ExchangeTemporaryCodeByTempCode code)


getServerVersion : Cmd Msg
getServerVersion =
    Http.send Msg.UpdateServerVersion WebApi.getApiV1Version


makeAuthorizationHeader : String -> WebApi.AuthzValue
makeAuthorizationHeader token =
    "Bearer " ++ token


getPortfolios : String -> Cmd Msg
getPortfolios token =
    let
        authzHeader =
            makeAuthorizationHeader token
    in
        Http.send Msg.UpdatePortfolios <|
            WebApi.getApiV1Portfolios authzHeader


getHistories : String -> WebApi.MarketCode -> Cmd Msg
getHistories token marketCode =
    let
        authzHeader =
            makeAuthorizationHeader token
    in
        Http.send Msg.UpdateAnalytics <|
            WebApi.getApiV1StocksHistoryByMarketCode authzHeader marketCode (Just "1d")


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
        initialModel =
            Model.initialModel flags.client_id

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
            ! [ getServerVersion
              , getWebApiDocument
              , exchangeCode
              , Material.init Msg.Mdl
              ]


main : Program Flags Model Msg
main =
    Navigation.programWithFlags Msg.UrlChange
        { init = init
        , view = View.view
        , subscriptions = subscriptions
        , update = update
        }
