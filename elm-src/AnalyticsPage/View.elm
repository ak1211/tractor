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
            [ Icon.i "insert_chart_outlined"
            , Options.span [ Options.css "width" "4px" ] []
            , Html.text "summary"
            ]
        , Tabs.label
            [ Options.center ]
            [ Icon.i "notes"
            , Options.span [ Options.css "width" "4px" ] []
            , Html.text "histories"
            ]
        ]
        [ case model.tab of
            1 ->
                historiesView model

            _ ->
                summaryView model
        ]


summaryView : AnalyticsPage.Model -> Html AnalyticsPage.Msg
summaryView model =
    let
        marketCode =
            Maybe.map (\q -> q.marketCode) model.quotes
                |> Maybe.withDefault "Not Available"

        contents =
            Html.dl []
                [ Html.dt [] [ Html.text "コード" ]
                , Html.dd [] [ Html.text marketCode ]
                ]

        width =
            "500px"
    in
        Grid.grid []
            [ Grid.cell [ Grid.size Grid.All 12 ]
                [ Html.p
                    [ Attr.style
                        [ ( "width", width )
                        , ( "margin", "0 auto" )
                        ]
                    ]
                    [ contents ]
                ]
            ]


historiesView : AnalyticsPage.Model -> Html AnalyticsPage.Msg
historiesView model =
    let
        hs =
            Maybe.map (\q -> q.histories) model.quotes
                |> Maybe.withDefault []
    in
        Grid.grid []
            [ Grid.cell [ Grid.size Grid.All 12 ]
                [ tableOhlcv hs ]
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
