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

import Http
import Maybe
import Navigation
import Material
import Model exposing (Model)
import Msg exposing (Msg)
import Route exposing (Route, TickerSymbol)
import View
import Task
import Dom.Scroll
import Material.Layout as Layout
import Generated.WebApi as Api


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg.DropZoneEntered ->
            { model | inDropZone = True } ! []

        Msg.DropZoneLeaved ->
            { model | inDropZone = False } ! []

        Msg.FilesDropped files ->
            { model | inDropZone = False, droppedFiles = files } ! []

        Msg.UrlChange location ->
            urlUpdate model location

        Msg.NewUrl url ->
            model ! [ Navigation.newUrl url ]

        Msg.UpdateServerVersion res ->
            { model | serverVersion = Result.toMaybe res } ! []

        Msg.UpdatePortfolios res ->
            { model | portfolios = Result.toMaybe res } ! []

        Msg.UpdateAnalytics res ->
            { model | histories = Result.toMaybe res } ! []

        Msg.UpdateWebApiDocument res ->
            { model | webApiDocument = Result.toMaybe res } ! []

        Msg.ScrollToTop ->
            model ! [ Task.attempt (always Msg.Nop) <| Dom.Scroll.toTop Layout.mainId ]

        Msg.Nop ->
            ( model, Cmd.none )

        -- Boilerplate: Mdl action handler.
        Msg.Mdl msg_ ->
            Material.update Msg.Mdl msg_ model


urlUpdate : Model -> Navigation.Location -> ( Model, Cmd Msg )
urlUpdate model location =
    case Route.fromLocation location of
        Just (Route.Analytics (Just ts)) ->
            let
                next =
                    Route.Analytics (Just ts)
            in
                { model | pageHistory = next :: model.pageHistory }
                    ! [ askHistories ts ]

        Just next ->
            { model | pageHistory = next :: model.pageHistory }
                ! [ Task.attempt (always Msg.Nop) <| Dom.Scroll.toTop Layout.mainId ]

        Nothing ->
            model ! []


askWebApiDocument : Cmd Msg
askWebApiDocument =
    Http.send Msg.UpdateWebApiDocument <|
        Http.getString "public/WebApiDocument.md"


askServerVersion : Cmd Msg
askServerVersion =
    Http.send Msg.UpdateServerVersion Api.getApiV1Version


askPortfolios : Cmd Msg
askPortfolios =
    Http.send Msg.UpdatePortfolios Api.getApiV1Portfolios


askHistories : TickerSymbol -> Cmd Msg
askHistories ts =
    Http.send Msg.UpdateAnalytics (Api.getApiV1QuotesByCode ts)


subscriptions : Model -> Sub Msg
subscriptions model =
    Material.subscriptions Msg.Mdl model


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        model : Model
        model =
            { count = 0
            , pageHistory =
                case Route.fromLocation location of
                    Just a ->
                        [ a ]

                    Nothing ->
                        []
            , inDropZone = False
            , droppedFiles = []
            , serverVersion = Nothing
            , portfolios = Nothing
            , histories = Nothing
            , webApiDocument = Nothing
            , mdl =
                Material.model

            -- Boilerplate: Always use this initial Mdl model store.
            }
    in
        model
            ! [ askServerVersion
              , askWebApiDocument
              , askPortfolios
              , Material.init Msg.Mdl
              ]


main : Program Never Model Msg
main =
    Navigation.program Msg.UrlChange
        { init = init
        , view = View.view
        , subscriptions = subscriptions
        , update = update
        }
