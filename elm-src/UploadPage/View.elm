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


module UploadPage.View exposing (view)

import FileReader
import Generated.WebApi as WebApi
import Html exposing (Html)
import Html.Attributes as Attr
import Material.Button as Button
import Material.Grid as Grid
import Material.Icon as Icon
import Material.List as Lists
import Material.Options as Options
import Material.Progress as Loading
import Material.Spinner as Loading
import Material.Table as Table
import Material.Toggles as Toggles
import Material.Typography as Typo
import UploadPage.Model as UploadPage
import UploadPage.Msg as UploadPage


viewCell : Maybe String -> String
viewCell =
    Maybe.withDefault "Not Available"


view : UploadPage.Model -> Html UploadPage.Msg
view model =
    Grid.grid
        []
        [ Grid.cell [ Grid.size Grid.All 12 ] [ dropZonePanel model ]
        , Grid.cell [ Grid.size Grid.All 12 ] [ controlPanel model ]
        , Grid.cell [ Grid.size Grid.All 12 ] [ fileContents model ]
        ]


dropZonePanel : UploadPage.Model -> Html UploadPage.Msg
dropZonePanel model =
    let
        bgColor =
            if model.inDropZone then
                "lightblue"
            else
                "gainsboro"

        opts =
            List.map Options.attribute <|
                [ Attr.style
                    [ ( "padding", "16px" )
                    , ( "line-height", "50px" )
                    , ( "background", bgColor )
                    ]
                ]
                    ++ dropZone

        dropZone =
            FileReader.dropZone
                { dataFormat = FileReader.Text "ms932"
                , enterMsg = UploadPage.DropZoneEntered
                , leaveMsg = UploadPage.DropZoneLeaved
                , filesMsg = UploadPage.FilesDropped
                }

        contents =
            [ Html.text "The CSV file to upload."
            , Html.br [] []
            , Html.text "Drop files here."
            ]
    in
        Options.styled
            Html.p
            (opts
                ++ [ Typo.title
                   , Typo.center
                   ]
            )
            contents


controlPanel : UploadPage.Model -> Html UploadPage.Msg
controlPanel model =
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
                if model.isPendingUpload then
                    [ Html.text (toString model.progress.counter)
                    , Html.text " / "
                    , Html.text (toString model.progress.total)
                    ]
                else
                    case List.head model.progress.done of
                        Just _ ->
                            [ Html.text "Done" ]

                        Nothing ->
                            [ Html.text "Let's upload." ]

        display =
            Options.styled Html.p [ Typo.body1 ] <|
                Maybe.withDefault [] <|
                    Maybe.map displayBody <|
                        if model.isPendingUpload then
                            List.head model.progress.done
                        else
                            Nothing

        displayBody x =
            let
                append =
                    if model.isStepByStepMode then
                        List.head model.progress.done
                            |> Maybe.map (\x -> x.ohlcvs)
                            |> Maybe.andThen List.head
                            |> Maybe.map (\x -> [ Html.text " / ", Html.text x.at ])
                    else
                        Nothing
            in
                List.concat
                    [ [ Html.text x.marketCode, Html.text " / ", Html.text x.timeFrame ]
                    , Maybe.withDefault [] append
                    , [ Html.text " is now sended." ]
                    ]

        isUploadable =
            let
                hasAccessToken =
                    case model.accessToken of
                        Just _ ->
                            True

                        Nothing ->
                            False

                hasOk x =
                    case x of
                        Ok _ ->
                            True

                        Err _ ->
                            False
            in
                hasAccessToken && List.any hasOk model.fileContents

        uploadButton enabled =
            Html.div
                [ Attr.style [ ( "margin-top", "16px" ) ] ]
                [ Button.render UploadPage.Mdl
                    [ 0 ]
                    model.mdl
                    [ Button.fab
                    , Button.colored
                    , if enabled then
                        Button.ripple
                      else
                        Button.disabled
                    , Options.onClick UploadPage.UploadAllFilesContent
                    ]
                    [ Icon.i "cloud_upload" ]
                ]

        uploadMode =
            Html.div
                []
                [ Options.styled
                    Html.p
                    [ Typo.headline ]
                    [ Html.text "upload mode" ]
                , Html.p []
                    [ Toggles.radio UploadPage.Mdl
                        [ 0 ]
                        model.mdl
                        [ Toggles.value (model.isStepByStepMode)
                        , Toggles.group "ModeGroup"
                        , Toggles.ripple
                        , Options.onToggle (UploadPage.ChangeStepByStepMode True)
                        ]
                        [ Html.text "Step By Step" ]
                    , Toggles.radio UploadPage.Mdl
                        [ 1 ]
                        model.mdl
                        [ Options.css "margin-left" "2em"
                        , Toggles.value (not model.isStepByStepMode)
                        , Toggles.group "ModeGroup"
                        , Toggles.ripple
                        , Options.onToggle (UploadPage.ChangeStepByStepMode False)
                        ]
                        [ Html.text "Batch" ]
                    ]
                ]

        style =
            Attr.style
                [ ( "margin", "16px auto" )
                , ( "max-width", "500px" )
                , ( "text-align", "center" )
                ]
    in
        Html.div [ style ]
            [ uploadMode
            , Html.div [ Attr.style [ ( "margin-top", "4em" ) ] ] []
            , display
            , numOfTotal
            , Loading.progress percent
            , Html.div [ Attr.style [ ( "margin", "16px" ) ] ]
                [ Loading.spinner [ Loading.active model.isPendingUpload ] ]
            , uploadButton (not model.isPendingUpload && isUploadable)
            ]


fileContents : UploadPage.Model -> Html UploadPage.Msg
fileContents model =
    Html.div [] <|
        List.map (viewFileContent model) model.fileContents


viewFileContent : UploadPage.Model -> Result String UploadPage.FileContent -> Html msg
viewFileContent model content =
    let
        margin =
            Options.css "margin" "0 auto"

        dijestContent data =
            Html.div [ Attr.style [ ( "margin", "auto" ), ( "max-width", "700px" ) ] ]
                [ Lists.ul []
                    [ Lists.li [ Lists.withSubtitle ]
                        [ Lists.content []
                            [ Lists.icon "label" []
                            , Html.text data.marketCode
                            , Lists.subtitle [] [ Html.text "コード(ファイル名より取得)" ]
                            ]
                        ]
                    , Lists.li [ Lists.withSubtitle ]
                        [ Lists.content []
                            [ Lists.icon "label" []
                            , Html.text data.name
                            , Lists.subtitle [] [ Html.text "銘柄名(ファイル内より取得)" ]
                            ]
                        ]
                    , Lists.li [ Lists.withSubtitle ]
                        [ Lists.content []
                            [ Lists.icon "label" []
                            , Html.text data.timeFrame
                            , Lists.subtitle [] [ Html.text "時間枠" ]
                            ]
                        ]
                    ]
                , Html.div [ Attr.style [ ( "text-align", "center" ) ] ]
                    [ tableOhlcv <| List.take 4 data.ohlcvs
                    , Icon.i "more_vert"
                    ]
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
