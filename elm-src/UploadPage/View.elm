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
   Module      :  UploadPage.View
   Description :  This file is web front main module of Application "Tractor"
   Copyright   :  (c) 2016 Akihiro Yamamoto
   License     :  AGPLv3

   Maintainer  :  https://github.com/ak1211
   Stability   :  unstable
   Portability :  POSIX

   Uploadページのビュー定義です。
-}


module UploadPage.View exposing (..)

import FileReader
import Generated.WebApi as WebApi
import Html exposing (Html)
import Html.Attributes as Attr
import Material.Button as Button
import Material.Grid as Grid
import Material.Options as Options
import Material.Progress as Loading
import Material.Table as Table
import Material.Typography as Typo
import UploadPage.Model as UploadPage
import UploadPage.Msg as UploadPage


viewCell : Maybe String -> String
viewCell =
    Maybe.withDefault "Not Available"


view : UploadPage.Model -> Html UploadPage.Msg
view model =
    let
        dropZone =
            Html.div
                ([ Attr.style
                    ([ ( "height", "8em" )
                     , ( "padding", "16px" )
                     ]
                        ++ (if model.inDropZone then
                                [ ( "background", "lightblue" ) ]
                            else
                                [ ( "background", "gainsboro" ) ]
                           )
                    )
                 ]
                    ++ FileReader.dropZone
                        { dataFormat = FileReader.Text "ms932"
                        , enterMsg = UploadPage.DropZoneEntered
                        , leaveMsg = UploadPage.DropZoneLeaved
                        , filesMsg = UploadPage.FilesDropped
                        }
                )
                [ Html.text "The CSV file to upload."
                , Html.p [] [ Html.text "Drop files here." ]
                ]

        fileContents =
            Html.p [ Attr.style [ ( "margin", "16px" ) ] ] <|
                List.map (viewFileContent model) model.fileContents

        controlPanel =
            let
                percent =
                    toFloat model.progress.counter
                        / toFloat model.progress.total
                        * 100.0

                numOfTotal =
                    Options.styled
                        Html.p
                        [ Typo.display1 ]
                    <|
                        if isEmpty then
                            [ Html.text "No Contents" ]
                        else
                            [ Html.text (toString model.progress.counter)
                            , Html.text " / "
                            , Html.text (toString model.progress.total)
                            ]

                show =
                    Options.styled Html.p [ Typo.body1 ] <|
                        case model.progress.done of
                            x :: _ ->
                                [ Html.text x.ohlcv.at
                                , Html.text " is now uploading."
                                ]

                            [] ->
                                []

                isUploadable =
                    let
                        hasOk x =
                            case x of
                                Ok _ ->
                                    True

                                Err _ ->
                                    False
                    in
                        List.any hasOk model.fileContents

                isEmpty =
                    List.isEmpty model.progress.todo && List.isEmpty model.progress.done

                uploadButton enabled =
                    Html.div
                        [ Attr.style [ ( "margin-top", "16px" ) ] ]
                        [ Button.render UploadPage.Mdl
                            [ 0 ]
                            model.mdl
                            [ Button.raised
                            , Button.colored
                            , if enabled then
                                Button.ripple
                              else
                                Button.disabled
                            , Options.onClick UploadPage.UploadAllFilesContent
                            ]
                            [ Html.text "Upload" ]
                        ]

                style =
                    Attr.style
                        [ ( "margin", "auto" )
                        , ( "max-width", "500px" )
                        , ( "text-align", "center" )
                        ]
            in
                Html.div [ style ]
                    [ show
                    , numOfTotal
                    , Loading.progress percent
                    , uploadButton
                        (if model.progress.counter == 0 then
                            isUploadable
                         else
                            False
                        )
                    ]
    in
        Grid.grid
            []
            [ Grid.cell [ Grid.size Grid.All 12 ] [ dropZone ]
            , Grid.cell [ Grid.size Grid.All 12 ] [ controlPanel ]
            , Grid.cell [ Grid.size Grid.All 12 ] [ fileContents ]
            ]


viewFileContent : UploadPage.Model -> Result String UploadPage.FileContent -> Html msg
viewFileContent model content =
    let
        dijestContent data =
            Html.div []
                [ Html.dl []
                    [ Html.dt [] [ Html.text "コード(ファイル名より取得)" ]
                    , Html.dd [] [ Html.text data.marketCode ]
                    , Html.dt [] [ Html.text "銘柄名(ファイル内より取得)" ]
                    , Html.dd [] [ Html.text data.name ]
                    , Html.dt [] [ Html.text "時間枠" ]
                    , Html.dd [] [ Html.text data.timeFrame ]
                    ]
                , Html.p [] [ tableOhlcv <| List.take 5 data.ohlcvs ]
                ]

        errorContent error =
            Html.p [] [ Html.text error ]
    in
        case content of
            Ok data ->
                dijestContent data

            Err error ->
                errorContent error


viewTable : List (Html m) -> List (Html m) -> Html m
viewTable headlines rows =
    Table.table []
        [ Table.thead [] [ Table.tr [] (List.map (\a -> Table.th [] [ a ]) headlines) ]
        , Table.tbody [] rows
        ]


tableOhlcv : List WebApi.ApiOhlcv -> Html msg
tableOhlcv data =
    let
        headlines =
            [ Html.text "時間"
            , Html.text "始値"
            , Html.text "高値"
            , Html.text "安値"
            , Html.text "終値"
            , Html.text "出来高"
            , Html.text "入手元"
            ]

        row : WebApi.ApiOhlcv -> Html msg
        row item =
            Table.tr []
                [ Table.td [ Table.numeric ] [ Html.text item.at ]
                , Table.td [ Table.numeric ] [ Html.text <| viewCell <| Maybe.map toString item.open ]
                , Table.td [ Table.numeric ] [ Html.text <| viewCell <| Maybe.map toString item.high ]
                , Table.td [ Table.numeric ] [ Html.text <| viewCell <| Maybe.map toString item.low ]
                , Table.td [ Table.numeric ] [ Html.text <| viewCell <| Maybe.map toString item.close ]
                , Table.td [ Table.numeric ] [ Html.text <| toString item.volume ]
                , Table.td [] [ Html.text <| viewCell item.source ]
                ]
    in
        viewTable headlines <| List.map row data