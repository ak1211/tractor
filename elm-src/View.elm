module View exposing (..)

import Html.Attributes as Attr
import Html exposing (Html)
import Csv exposing (Csv)
import Dict exposing (Dict)
import Markdown
import Material
import MimeType
import Material.Button as Button
import Material.Card as Card
import Material.Typography as Typo
import Material.Color as Color
import Material.Dialog as Dialog
import Material.Elevation as Elevation
import Material.Table as Table
import Material.Grid as Grid
import Material.Icon as Icon
import Material.Layout as Layout
import Material.Options as Options
import Material.Scheme
import FileReader
import Model exposing (Model)
import Msg exposing (Msg)
import Route exposing (Route)
import Generated.WebApi as WebApi


-- Define the dialog


dialogAbout : Model -> Html Msg
dialogAbout model =
    Dialog.view
        []
        [ Dialog.title [] [ Html.text "ABOUT" ]
        , Dialog.content []
            [ Html.p [] [ Html.text "A front end of TRACTOR application." ]
            ]
        , Dialog.actions []
            [ Button.render Msg.Mdl
                [ 0 ]
                model.mdl
                [ Dialog.closeOn "click" ]
                [ Html.text "close" ]
            ]
        ]


pinkCard : List (Html m) -> Html m
pinkCard contents =
    Card.view
        [ Options.css "width" "200px"
        , Options.css "margin" "10px"
        , Color.background <| Color.color Color.Pink Color.S500
        ]
        [ Card.text [ Color.text Color.white ] contents
        ]



-- VIEW


type alias Mdl =
    Material.Model


view : Model -> Html Msg
view model =
    Layout.render Msg.Mdl
        model.mdl
        [ Layout.fixedHeader
        , Layout.fixedDrawer
        , Layout.waterfall True
        , Options.css "display" "flex !important"
        , Options.css "flex-direction" "row"
        , Options.css "align-items" "center"
        , Options.css "align-items" "center"
        ]
        { header = [ viewHeader model ]
        , drawer = [ drawerHeader model, viewDrawer model ]
        , tabs = ( [], [] )
        , main = [ viewBody model, dialogAbout model ]
        }
        |> Material.Scheme.topWithScheme Color.Lime Color.Indigo


viewHeader : Model -> Html Msg
viewHeader model =
    let
        pageText =
            List.head model.pageHistory
                |> Maybe.map Route.routeCaption
                |> Maybe.withDefault ""

        hrefGitHub =
            Layout.href "https://github.com/ak1211/tractor"

        imgGitHub =
            Html.img [ Attr.src "public/assets/GitHub-Mark-32px.png" ] []
    in
        Layout.row
            [ Color.background <| Color.color Color.Amber Color.S300
            , Color.text <| Color.color Color.BlueGrey Color.S700
            ]
            [ Layout.title
                [ Options.center ]
                [ Html.text "TRACTOR"
                , Icon.i "chevron_right"
                , Html.text pageText
                ]
            , Layout.spacer
            , Layout.navigation [] [ Layout.link [ hrefGitHub ] [ imgGitHub ] ]
            ]


type alias MenuItem =
    { iconName : String
    , route : Route
    }


menuItems : List MenuItem
menuItems =
    [ { iconName = "dashboard", route = Route.Dashboard }
    , { iconName = "file_upload", route = Route.Upload }
    , { iconName = "list", route = Route.Portfolio }
    , { iconName = "show_chart", route = Route.Analytics }
    , { iconName = "note", route = Route.Reports }
    , { iconName = "account_balance", route = Route.AccBalance }
    , { iconName = "school", route = Route.ApiDocument }
    ]


viewDrawerMenuItem : Model -> MenuItem -> Html Msg
viewDrawerMenuItem model menuItem =
    let
        isCurrentLocation =
            case List.head model.pageHistory of
                Just currentLocation ->
                    currentLocation == menuItem.route

                Nothing ->
                    False

        onClickCmd =
            case isCurrentLocation of
                True ->
                    Options.nop

                False ->
                    menuItem.route |> Route.toUrlPath |> Msg.NewUrl |> Options.onClick
    in
        Layout.link
            [ onClickCmd
            , Color.text <| Color.color Color.BlueGrey Color.S100
            , Options.when
                isCurrentLocation
                (Color.background <| Color.color Color.BlueGrey Color.S500)
            , Options.css "color" "rgba(255, 255, 255, 0.56)"
            , Options.css "font-weight" "500"
            ]
            [ Icon.view menuItem.iconName
                [ Color.text <| Color.color Color.BlueGrey Color.S100
                , Options.css "margin-right" "26px"
                ]
            , Html.text <| Route.routeCaption menuItem.route
            ]


viewDrawer : Model -> Html Msg
viewDrawer model =
    Layout.navigation
        [ Color.background <| Color.color Color.BlueGrey Color.S800
        , Options.css "flex-grow" "1"
        , Options.css "padding" "0"
        ]
    <|
        (List.map (viewDrawerMenuItem model) menuItems)
            ++ [ Layout.spacer
               , Layout.link
                    [ Dialog.openOn "click"
                    ]
                    [ Icon.view "help"
                        [ Color.text <| Color.color Color.BlueGrey Color.S100
                        ]
                    ]
               ]


drawerHeader : Model -> Html Msg
drawerHeader model =
    Options.styled Html.header
        [ Options.css "display" "flex"
        , Options.css "box-sizing" "border-box"
        , Options.css "justify-content" "flex-end"
        , Options.css "padding" "20px"
        , Options.css "height" "64px"
        , Options.css "flex-direction" "column"
        , Options.css "font-family" "'Gugi', cursive"
        , Options.css "font-size" "2.0rem"
        , Options.css "letter-spacing" "0.25em"
        , Options.cs "drawer-header"
        , Color.background <| Color.color Color.BlueGrey Color.S900
        , Color.text <| Color.color Color.Amber Color.S300
        , Options.center
        , Route.toUrlPath Route.Dashboard |> Msg.NewUrl |> Options.onClick
        ]
        [ Html.text "TRACTOR" ]


viewBody : Model -> Html Msg
viewBody model =
    let
        page =
            Maybe.withDefault Route.Dashboard <|
                List.head model.pageHistory
    in
        case page of
            Route.Dashboard ->
                viewDashboard model

            Route.Upload ->
                viewUpload model

            Route.Portfolio ->
                viewPortfolio model

            Route.Analytics ->
                viewAnalytics model

            Route.Reports ->
                viewReports model

            Route.AccBalance ->
                pinkCard [ Html.text "No Contents here." ]

            Route.ApiDocument ->
                viewApiDocument model


viewDashboard : Model -> Html Msg
viewDashboard model =
    let
        card head subhead contents =
            Card.view
                [ Elevation.e4
                , Options.css "width" "100%"
                , Options.css "height" "100%"
                , Options.css "position" "relative"
                , Options.css "word-wrap" "break-word"
                , Color.background <| Color.color Color.Amber Color.S50
                ]
                [ Card.text [ Options.css "padding-bottom" "90px" ] contents
                , Card.title
                    [ Color.background <| Color.primary
                    , Color.text Color.black
                    , Options.css "width" "100%"
                    , Options.css "position" "absolute"
                    , Options.css "bottom" "0"
                    ]
                    [ Card.head
                        []
                        [ Options.styled
                            Html.div
                            [ Typo.display1
                            , Typo.uppercase
                            ]
                            [ Html.text head ]
                        ]
                    , Card.subhead
                        [ Options.css "margin-top" "-10px"
                        , Typo.title
                        ]
                        [ Html.text subhead ]
                    ]
                ]

        viewVersion v =
            [ Options.styled
                Html.p
                [ Typo.title ]
                [ Html.text "Server version is" ]
            , Options.styled
                Html.p
                [ Typo.display3
                , Typo.center
                ]
                [ Html.text v.version ]
            , Options.styled
                Html.p
                [ Typo.title
                , Typo.right
                ]
                [ Html.text <| "(" ++ v.gitStatus ++ ")" ]
            ]

        viewRevision v =
            [ Options.styled
                Html.p
                [ Typo.title ]
                [ Html.text "Git hash is" ]
            , Options.styled
                Html.p
                [ Typo.body1, Typo.center ]
                [ Html.text v.gitHash ]
            , Options.styled
                Html.p
                [ Typo.title ]
                [ Html.text "Git commit date is" ]
            , Options.styled
                Html.p
                [ Typo.body1, Typo.center ]
                [ Html.text v.gitCommitDate ]
            ]

        viewNA =
            Html.text "No datasets available"
    in
        Grid.grid
            []
            [ Grid.cell
                [ Grid.size Grid.All 4 ]
                [ model.serverVersion
                    |> Maybe.map (card "Version" "of server" << viewVersion)
                    |> Maybe.withDefault viewNA
                ]
            , Grid.cell
                [ Grid.size Grid.All 4 ]
                [ model.serverVersion
                    |> Maybe.map (card "Revision" "of server" << viewRevision)
                    |> Maybe.withDefault viewNA
                ]
            ]


viewCell : Maybe String -> String
viewCell =
    Maybe.withDefault "Not Available"


viewUpload : Model -> Html Msg
viewUpload model =
    {-
       div
           [ style [ ( "padding", "2rem" ) ] ]
           [ text ("upload file : " ++ toString model.count)
           , Button.render Mdl
               [ 0 ]
               model.mdl
               [ Options.onClick Increase
               , Options.css "margin" "0 24px"
               ]
               [ text "Increase" ]
           , Button.render Mdl
               [ 1 ]
               model.mdl
               [ Options.onClick Reset ]
               [ text "Reset" ]
           ]
    -}
    Grid.grid
        []
        [ Grid.cell
            [ Grid.size Grid.All 12 ]
            [ Html.div
                ([ Attr.style
                    ([ ( "width", "300px" )
                     , ( "height", "300px" )
                     , ( "margin", "auto" )
                     , ( "padding", "16px" )
                     , ( "border", "solid medium slategrey" )
                     ]
                        ++ (if model.inDropZone then
                                [ ( "background", "lightblue" ) ]
                            else
                                []
                           )
                    )
                 ]
                    ++ FileReader.dropZone
                        { dataFormat = FileReader.Text "ms932"
                        , enterMsg = Msg.DropZoneEntered
                        , leaveMsg = Msg.DropZoneLeaved
                        , filesMsg = Msg.FilesDropped
                        }
                )
                [ Html.text "The CSV file to uplode."
                , Html.p [] [ Html.text "Drop files here." ]
                ]
            , if List.length model.droppedFiles > 0 then
                Html.div [] (List.map viewFile model.droppedFiles)
              else
                Html.text ""
            ]
        ]


viewFile : FileReader.File -> Html Msg
viewFile file =
    let
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

        toOhlcvt : String -> Dict String String -> WebApi.Ohlcv
        toOhlcvt marketCode_ recs =
            let
                toNumber : (String -> Result a b) -> String -> Maybe b
                toNumber fn name =
                    Dict.get name recs
                        |> Maybe.andThen (Result.toMaybe << fn << String.trim)

                toFloat : String -> Maybe Float
                toFloat =
                    toNumber String.toFloat

                toInt : String -> Maybe Int
                toInt =
                    toNumber String.toInt

                date : Maybe String
                date =
                    Dict.get "Date" recs

                time : Maybe String
                time =
                    Dict.get "Time" recs

                datetime : String
                datetime =
                    Maybe.map2 (\a b -> a ++ "T" ++ b ++ "+0900") date time
                        |> Maybe.withDefault "Date and Time parse error"
            in
                { at = datetime
                , code = marketCode_
                , open = toFloat "Open"
                , high = toFloat "High"
                , low = toFloat "Low"
                , close = toFloat "Close"
                , volume = toInt "Volume" |> Maybe.withDefault 0
                , source = Just "matsui.co.jp"
                }

        marketCode =
            -- 今はファイル名を使う事にする
            String.split "." file.name |> List.head |> Maybe.map String.trim |> Maybe.withDefault ""

        viewFileData =
            case file.data of
                Ok data ->
                    case ( file.dataFormat, MimeType.parseMimeType file.mimeType ) of
                        ( FileReader.DataURL, Just (MimeType.Image _) ) ->
                            [ Html.img [ Attr.src data ] [] ]

                        _ ->
                            let
                                ( name, recs ) =
                                    parseFromNetstockCsv data
                            in
                                [ List.map (toOhlcvt marketCode) recs |> tableOhlcv ]

                Err error ->
                    [ Html.text ("Error: " ++ toString error.code ++ " " ++ error.name ++ " " ++ error.message) ]
    in
        Html.div [] <|
            [ Html.dl []
                [ Html.dt [] [ Html.text "File name" ]
                , Html.dd [] [ Html.text file.name ]
                , Html.dt [] [ Html.text "File size" ]
                , Html.dd [] [ Html.text (toString file.size) ]
                , Html.dt [] [ Html.text "Last modified" ]
                , Html.dd [] [ Html.text (toString file.lastModified) ]
                , Html.dt [] [ Html.text "Mime type" ]
                , Html.dd [] [ Html.text file.mimeType ]
                , Html.dt [] [ Html.text "Data format" ]
                , Html.dd [] [ Html.text (toString file.dataFormat) ]
                , Html.dt [] [ Html.text "Data" ]
                ]
            ]
                ++ viewFileData


viewTable : List (Html m) -> (a -> Html m) -> List a -> Html m
viewTable headlines viewRow data =
    Table.table []
        [ Table.thead []
            [ Table.tr [] (List.map (\a -> Table.th [] [ a ]) headlines)
            ]
        , Table.tbody [] <| List.map viewRow data
        ]


viewPortfolio : Model -> Html Msg
viewPortfolio model =
    let
        viewRow : WebApi.Portfolio -> Html Msg
        viewRow item =
            Table.tr [ Options.onClick <| Msg.RequestNewHistories item.code ]
                [ Table.td [ Table.numeric ] [ Html.text item.code ]
                , Table.td [] [ Html.text <| viewCell item.caption ]
                , Table.td [ Table.numeric ] [ Html.text <| viewCell item.updateAt ]
                ]

        headlines =
            [ Html.text "証券コード"
            , Html.text "銘柄名"
            , Html.text "更新時間"
            ]
    in
        case model.portfolios of
            Just a ->
                viewTable headlines viewRow a

            Nothing ->
                pinkCard [ Html.text "No datasets available" ]


tableOhlcv : List WebApi.Ohlcv -> Html Msg
tableOhlcv data =
    let
        viewRow : WebApi.Ohlcv -> Html Msg
        viewRow item =
            Table.tr []
                [ Table.td [] [ Html.text item.code ]
                , Table.td [ Table.numeric ] [ Html.text item.at ]
                , Table.td [ Table.numeric ] [ Html.text <| viewCell <| Maybe.map toString item.open ]
                , Table.td [ Table.numeric ] [ Html.text <| viewCell <| Maybe.map toString item.high ]
                , Table.td [ Table.numeric ] [ Html.text <| viewCell <| Maybe.map toString item.low ]
                , Table.td [ Table.numeric ] [ Html.text <| viewCell <| Maybe.map toString item.close ]
                , Table.td [ Table.numeric ] [ Html.text <| toString item.volume ]
                , Table.td [] [ Html.text <| viewCell item.source ]
                ]

        headlines =
            [ Html.text "証券コード"
            , Html.text "時間"
            , Html.text "始値"
            , Html.text "高値"
            , Html.text "安値"
            , Html.text "終値"
            , Html.text "出来高"
            , Html.text "入手元"
            ]
    in
        viewTable headlines viewRow data


viewAnalytics : Model -> Html Msg
viewAnalytics model =
    let
        def =
            pinkCard [ Html.text "No datasets available" ]
    in
        model.histories |> Maybe.map tableOhlcv |> Maybe.withDefault def


viewReports : Model -> Html Msg
viewReports model =
    pinkCard [ Html.text "No Reports here." ]


viewApiDocument : Model -> Html Msg
viewApiDocument model =
    let
        attributes =
            [ Attr.class "content"
            , Attr.style [ ( "margin", "20px" ) ]
            ]
    in
        Markdown.toHtml attributes model.webApiDocumentMd
