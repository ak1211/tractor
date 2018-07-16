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
   Module      :  AnalyticsPage.View
   Description :  This file is web front main module of Application "Tractor"
   Copyright   :  (c) 2016 Akihiro Yamamoto
   License     :  AGPLv3

   Maintainer  :  https://github.com/ak1211
   Stability   :  unstable
   Portability :  POSIX

   Analyticsページのビュー定義です。
-}


module AnalyticsPage.View exposing (view)

import AnalyticsPage.Model as AnalyticsPage
import AnalyticsPage.Msg as AnalyticsPage
import Generated.WebApi as WebApi
import Html exposing (Html)
import Html.Attributes as Attr
import Material.Color as Color
import Material.Grid as Grid
import Material.Icon as Icon
import Material.Options as Options
import Material.Table as Table
import Material.Tabs as Tabs


view : AnalyticsPage.Model -> Maybe WebApi.MarketCode -> Html AnalyticsPage.Msg
view model marketCode =
    Tabs.render AnalyticsPage.Mdl
        [ 0 ]
        model.mdl
        [ Tabs.ripple
        , Tabs.onSelectTab AnalyticsPage.SelectTab
        , Tabs.activeTab model.tab
        ]
        [ Tabs.label
            [ Options.center ]
            [ Icon.i "widgets"
            , Options.span [ Options.css "width" "4px" ] []
            , Html.text "summary"
            ]
        , Tabs.label
            [ Options.center ]
            [ Icon.i "insert_chart_outlined"
            , Options.span [ Options.css "width" "4px" ] []
            , Html.text "chart"
            ]
        , Tabs.label
            [ Options.center ]
            [ Icon.i "notes"
            , Options.span [ Options.css "width" "4px" ] []
            , Html.text "histories"
            ]
        ]
        [ case model.tab of
            2 ->
                historiesView model

            1 ->
                chartView model

            _ ->
                summaryView model
        ]


chartURI : WebApi.MarketCode -> ( Int, Int ) -> String
chartURI mc ( w, h ) =
    "https://tractor.ak1211.com/api/v1/stocks/chart/"
        ++ mc
        ++ ".svg?tf=%221d%22"
        ++ "&w="
        ++ toString w
        ++ "&h="
        ++ toString h


summaryView : AnalyticsPage.Model -> Html AnalyticsPage.Msg
summaryView model =
    let
        contents mc =
            Grid.grid []
                [ Grid.cell
                    [ Grid.size Grid.Desktop 3
                    , Grid.size Grid.Tablet 4
                    , Color.background <| Color.color Color.Brown Color.S50
                    , Options.css "margin" "10px"
                    , Options.css "padding" "10px"
                    ]
                    [ Html.div []
                        [ Html.p []
                            [ Html.dl []
                                [ Html.dt [] [ Html.text "コード" ]
                                , Html.dd [] [ Html.text mc ]
                                ]
                            ]
                        ]
                    ]
                , Grid.cell
                    [ Grid.size Grid.Desktop 3
                    , Grid.size Grid.Tablet 4
                    , Color.background <| Color.color Color.Brown Color.S50
                    , Options.css "margin" "10px"
                    , Options.css "padding" "10px"
                    ]
                    [ Html.div []
                        [ Html.img
                            [ Attr.src <| chartURI mc ( 500, 500 )
                            , Attr.style [ ( "width", "100%" ) ]
                            ]
                            []
                        ]
                    ]
                ]
    in
        case model.marketCode of
            Nothing ->
                Grid.grid [] [ Grid.cell [] [ Html.text "No Contents" ] ]

            Just mc ->
                contents mc


chartView : AnalyticsPage.Model -> Html AnalyticsPage.Msg
chartView model =
    let
        contents mc =
            Grid.grid []
                [ Grid.cell
                    [ Grid.size Grid.All 12
                    , Color.background <| Color.color Color.Brown Color.S50
                    , Options.css "margin" "10px"
                    , Options.css "padding" "10px"
                    ]
                    [ Html.div []
                        [ Html.img
                            [ Attr.src <| chartURI mc ( 1200, 400 )
                            , Attr.style [ ( "height", "100%" ) ]
                            ]
                            []
                        ]
                    ]
                ]
    in
        case model.marketCode of
            Nothing ->
                Grid.grid [] []

            Just mc ->
                contents mc


historiesView : AnalyticsPage.Model -> Html AnalyticsPage.Msg
historiesView model =
    Grid.grid []
        [ Grid.cell [ Grid.size Grid.Desktop 12 ]
            [ tableOhlcv model.histories ]
        ]


viewCell : Maybe String -> String
viewCell =
    Maybe.withDefault "Not Available"


viewTable : List (Html m) -> List (Html m) -> Html m
viewTable headlines rows =
    Table.table [ Options.css "margin" "0 auto" ]
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
