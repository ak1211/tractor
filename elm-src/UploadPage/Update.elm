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
   Module      :  UploadPage.Update
   Description :  This file is web front main module of Application "Tractor"
   Copyright   :  (c) 2016 Akihiro Yamamoto
   License     :  AGPLv3

   Maintainer  :  https://github.com/ak1211
   Stability   :  unstable
   Portability :  POSIX

   Uploadページのアップデートモジュールです。
-}


module UploadPage.Update exposing (..)

import Http
import Csv exposing (Csv)
import FileReader
import Task
import Dict exposing (Dict)
import Generated.WebApi as WebApi
import Material
import UploadPage.Model as UploadPage
import UploadPage.Msg as UploadPage


update : UploadPage.Msg -> UploadPage.Model -> ( UploadPage.Model, Cmd UploadPage.Msg )
update msg model =
    case msg of
        UploadPage.ChangeAccessToken newToken ->
            { model | accessToken = newToken } ! []

        UploadPage.DropZoneEntered ->
            { model | inDropZone = True } ! []

        UploadPage.DropZoneLeaved ->
            { model | inDropZone = False } ! []

        UploadPage.FilesDropped files ->
            let
                cs =
                    List.map toUploadFileContent files
            in
                { model
                    | inDropZone = False
                    , fileContents = cs
                    , progress = UploadPage.initialUploadProgress
                }
                    ! []

        UploadPage.UploadAllFilesContent ->
            case model.accessToken of
                Just token ->
                    let
                        okFiles : List UploadPage.FileContent
                        okFiles =
                            List.filterMap Result.toMaybe model.fileContents

                        makeTodo : WebApi.MarketCode -> WebApi.TimeFrame -> WebApi.ApiOhlcv -> UploadPage.UploadThunk
                        makeTodo mc tf v =
                            { function = putOhlcv token mc tf
                            , ohlcv = v
                            }

                        makeTodoList : UploadPage.FileContent -> List UploadPage.UploadThunk
                        makeTodoList fileContent =
                            List.map (makeTodo fileContent.marketCode fileContent.timeFrame) fileContent.ohlcvs

                        newUploadProgress : UploadPage.UploadProgress
                        newUploadProgress =
                            let
                                todoList =
                                    List.concat (List.map makeTodoList okFiles)
                            in
                                { counter = 0
                                , total = List.length todoList
                                , todo = todoList
                                , done = []
                                }

                        newModel =
                            { model
                                | fileContents = []
                                , progress = newUploadProgress
                            }
                    in
                        newModel ! [ Task.perform identity <| Task.succeed UploadPage.UploadContent ]

                Nothing ->
                    model ! []

        UploadPage.UploadContent ->
            case model.progress.todo of
                x :: xs ->
                    let
                        progress =
                            model.progress

                        next =
                            { progress
                                | counter = progress.counter + 1
                                , todo = xs
                                , done = x :: progress.done
                            }
                    in
                        { model | progress = next } ! [ x.function x.ohlcv ]

                [] ->
                    model ! []

        UploadPage.DoneUploadContent _ ->
            model ! [ Task.perform identity <| Task.succeed UploadPage.UploadContent ]

        UploadPage.Mdl subMsg ->
            Material.update UploadPage.Mdl subMsg model


toUploadFileContent : FileReader.File -> Result String UploadPage.FileContent
toUploadFileContent file =
    let
        marketCode : Result String WebApi.MarketCode
        marketCode =
            -- 今はファイル名を使う事にする
            String.split "." file.name
                |> List.head
                |> Maybe.map String.trim
                |> Result.fromMaybe "CSVファイル名からマーケットコードを得られませんでした"

        detectTimeFrame records =
            "1d"

        pack name mc records =
            let
                tf =
                    detectTimeFrame records
            in
                { name = name
                , marketCode = mc
                , timeFrame = tf
                , ohlcvs = List.map (toOhlcvt tf) records
                }
    in
        case file.data of
            Ok data ->
                case parseFromNetstockCsv data of
                    ( Just name, records ) ->
                        marketCode |> Result.map (\mc -> pack name mc records)

                    ( Nothing, _ ) ->
                        Err "CSVファイルのパースに失敗しました"

            Err error ->
                Err ("parse error of " ++ file.name)


parseFromNetstockCsv : String -> ( Maybe String, List (Dict String String) )
parseFromNetstockCsv data =
    let
        lines =
            Csv.split data

        name =
            List.take 1 lines |> List.take 1 |> List.concat |> List.head

        hds =
            List.drop 1 lines |> List.take 1 |> List.concat

        recs =
            List.drop 2 lines

        toDict : List String -> Dict String String
        toDict r =
            List.map2 (\k v -> ( k, v )) hds r
                |> Dict.fromList
    in
        ( name, List.map toDict recs )


toOhlcvt : WebApi.TimeFrame -> Dict String String -> WebApi.ApiOhlcv
toOhlcvt tf records =
    let
        toNumber : (String -> Result a b) -> String -> Maybe b
        toNumber fn name =
            Dict.get name records
                |> Maybe.andThen (Result.toMaybe << fn << String.trim)

        toFloat : String -> Maybe Float
        toFloat =
            toNumber String.toFloat

        toInt : String -> Maybe Int
        toInt =
            toNumber String.toInt

        date : Maybe String
        date =
            Dict.get "Date" records

        time : Maybe String
        time =
            Dict.get "Time" records

        datetime : String
        datetime =
            Maybe.withDefault "Date and Time parse error" <|
                case tf of
                    "1d" ->
                        Maybe.map (\a -> a ++ "T15:00:00+0900") date

                    _ ->
                        Maybe.map2 (\a b -> a ++ "T" ++ b ++ "+0900") date time
    in
        { at = datetime
        , open = toFloat "Open"
        , high = toFloat "High"
        , low = toFloat "Low"
        , close = toFloat "Close"
        , volume = toInt "Volume" |> Maybe.withDefault 0
        , source = Just "matsui.co.jp"
        }


makeAuthorizationHeader : WebApi.AccessToken -> WebApi.AuthzValue
makeAuthorizationHeader token =
    "Bearer " ++ token


putOhlcv : WebApi.AccessToken -> WebApi.MarketCode -> WebApi.TimeFrame -> WebApi.ApiOhlcv -> Cmd UploadPage.Msg
putOhlcv token marketCode timeFrame ohlcv =
    let
        authzHeader =
            makeAuthorizationHeader token

        x =
            Debug.log "" ohlcv
    in
        WebApi.putApiV1StocksHistoryByMarketCode
            authzHeader
            marketCode
            (Just timeFrame)
            [ ohlcv ]
            |> Http.send UploadPage.DoneUploadContent
