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
import Route exposing (Route)
import View
import Task
import Dom.Scroll
import Material.Layout as Layout
import Generated.WebApi as Api


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg.Increase ->
            ( { model | count = model.count + 1 }
            , Cmd.none
            )

        Msg.Reset ->
            ( { model | count = 0 }
            , Cmd.none
            )

        Msg.DropZoneEntered ->
            ( { model | inDropZone = True }
            , Cmd.none
            )

        Msg.DropZoneLeaved ->
            ( { model | inDropZone = False }
            , Cmd.none
            )

        Msg.FilesDropped files ->
            ( { model | inDropZone = False, droppedFiles = files }
            , Cmd.none
            )

        Msg.UrlChange location ->
            location |> Route.fromLocation |> urlUpdate model

        Msg.NewUrl url ->
            ( model
            , Navigation.newUrl url
            )

        Msg.NewServerVersion (Ok version) ->
            ( { model | serverVersion = Just version }
            , Cmd.none
            )

        Msg.NewServerVersion (Err a) ->
            ( { model | serverVersion = Nothing }
            , Cmd.none
            )

        Msg.NewPortfolios (Ok pf) ->
            ( { model | portfolios = Just pf }
            , Cmd.none
            )

        Msg.NewPortfolios (Err _) ->
            ( { model | portfolios = Nothing }
            , Cmd.none
            )

        Msg.NewHistories (Ok hs) ->
            update
                (Route.toUrlPath Route.Analytics |> Msg.NewUrl)
                { model | histories = Just hs }

        Msg.NewHistories (Err _) ->
            ( { model | histories = Nothing }
            , Cmd.none
            )

        Msg.NewWebApiDocumentMd (Ok md) ->
            ( { model | webApiDocumentMd = md }
            , Cmd.none
            )

        Msg.NewWebApiDocumentMd (Err _) ->
            ( { model | webApiDocumentMd = "Document file load error." }
            , Cmd.none
            )

        Msg.RequestNewHistories code ->
            model ! [ getHistories code ]

        Msg.ScrollToTop ->
            ( model
            , Task.attempt (always Msg.Nop) <| Dom.Scroll.toTop Layout.mainId
            )

        Msg.Nop ->
            model ! []

        -- Boilerplate: Mdl action handler.
        Msg.Mdl msg_ ->
            Material.update Msg.Mdl msg_ model


urlUpdate : Model -> Maybe Route -> ( Model, Cmd Msg )
urlUpdate model route =
    case route of
        Just next ->
            ( { model | pageHistory = next :: model.pageHistory }
            , Task.attempt (always Msg.Nop) <| Dom.Scroll.toTop Layout.mainId
            )

        Nothing ->
            model ! []


getWebApiDocumentMd : Cmd Msg
getWebApiDocumentMd =
    let
        get =
            Http.getString "public/WebApiDocument.md"
    in
        Http.send Msg.NewWebApiDocumentMd get


getServerVersion : Cmd Msg
getServerVersion =
    Http.send Msg.NewServerVersion Api.getApiV1Version


getPortfolios : Cmd Msg
getPortfolios =
    Http.send Msg.NewPortfolios Api.getApiV1Portfolios


getHistories : String -> Cmd Msg
getHistories code =
    Http.send Msg.NewHistories (Api.getApiV1QuotesByCode code)


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
            , webApiDocumentMd = ""
            , mdl =
                Material.model

            -- Boilerplate: Always use this initial Mdl model store.
            }

        cmd =
            Cmd.batch
                [ getServerVersion
                , getPortfolios
                , getWebApiDocumentMd
                , Material.init Msg.Mdl
                ]
    in
        ( model, cmd )


main : Program Never Model Msg
main =
    Navigation.program Msg.UrlChange
        { init = init
        , view = View.view
        , subscriptions = subscriptions
        , update = update
        }
