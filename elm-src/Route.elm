module Route exposing (Route(..), routeCaption, toUrlPath, fromLocation)

import Navigation
import UrlParser
import List


type Route
    = Dashboard
    | Upload
    | Portfolio
    | Analytics
    | Reports
    | AccBalance
    | ApiDocument


type alias Item =
    { caption : String
    , path : String
    }


forwardLookup : Route -> Item
forwardLookup rt =
    case rt of
        Dashboard ->
            Item "Dashboard" ""

        Upload ->
            Item "Upload" "upload"

        Portfolio ->
            Item "Portfolio" "portfolio"

        Analytics ->
            Item "Analytics" "analytics"

        Reports ->
            Item "Reports" "reports"

        AccBalance ->
            Item "Account Balance" "acc-balance"

        ApiDocument ->
            Item "API document" "api-document"


routeCaption : Route -> String
routeCaption route =
    (forwardLookup route).caption


toUrlPath : Route -> String
toUrlPath route =
    "/" ++ (forwardLookup route).path


fromLocation : Navigation.Location -> Maybe Route
fromLocation path =
    UrlParser.parsePath parser path


parser : UrlParser.Parser (Route -> a) a
parser =
    let
        f route =
            UrlParser.map route <| UrlParser.s <| (forwardLookup route).path

        paths =
            List.map f
                [ Upload
                , Portfolio
                , Analytics
                , Reports
                , AccBalance
                , ApiDocument
                ]

        top =
            UrlParser.map Dashboard UrlParser.top
    in
        UrlParser.oneOf (top :: paths)
