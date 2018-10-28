{-
   https://github.com/rtfeldman/elm-spa-example

   Copyright (c) 2017-2018 Richard Feldman and contributors
   see https://github.com/rtfeldman/elm-spa-example/blob/master/LICENSE

   Changes since Oct. 2018 copyright &copy; by Akihiro Yamamoto
   see https://github.com/ak1211/tractor/blob/master/LICENSE
-}


module Route exposing (AuthRedirectParam, AuthRedirectResult, Route(..), fromUrl, href, replaceUrl)

import Api.Endpoint as Endpoint
import Basics
import Browser.Navigation as Navigation
import Html exposing (Attribute)
import Html.Attributes as Attrib
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>), Parser)
import Url.Parser.Query as Query


type alias AuthRedirectParam =
    { code : Endpoint.AuthTempCode, state : String }


type alias AuthRedirectResult =
    Result String AuthRedirectParam



-- ROUTING


type Route
    = Root
    | Dashboard
    | Login (Maybe AuthRedirectResult)
    | Logout
    | Upload
    | Portfolio
    | Reports
    | AccountBalance
    | ApiDocument


parser : Parser (Route -> a) a
parser =
    let
        handleRoot err code state =
            case ( err, code, state ) of
                ( Just e, _, _ ) ->
                    let
                        result =
                            Err e
                    in
                    Login (Just result)

                ( Nothing, Just c, Just s ) ->
                    let
                        result =
                            Ok
                                { code = Endpoint.AuthTempCode c
                                , state = s
                                }
                    in
                    Login (Just result)

                _ ->
                    Root
    in
    Parser.oneOf
        [ Parser.map handleRoot <|
            Parser.top
                <?> Query.string "error"
                <?> Query.string "code"
                <?> Query.string "state"
        , Parser.map Dashboard (Parser.s "dashboard")
        , Parser.map (Login Nothing) (Parser.s "login")
        , Parser.map Logout (Parser.s "logout")
        , Parser.map Upload (Parser.s "upload")
        , Parser.map Portfolio (Parser.s "portfolio")
        , Parser.map Reports (Parser.s "reports")
        , Parser.map AccountBalance (Parser.s "account-balance")
        , Parser.map ApiDocument (Parser.s "api-document")
        ]



-- PUBLIC HELPERS


href : Route -> Attribute msg
href targetRoute =
    Attrib.href (routeToString targetRoute)


replaceUrl : Navigation.Key -> Route -> Cmd msg
replaceUrl key route =
    Navigation.replaceUrl key (routeToString route)


fromUrl : Url -> Maybe Route
fromUrl url =
    -- The RealWorld spec treats the fragment like a path.
    -- This makes it *literally* the path, so we can proceed
    -- with parsing as if it had been a normal path all along.
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser



-- INTERNAL


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Dashboard ->
                    [ "dashboard" ]

                Root ->
                    []

                Login _ ->
                    [ "login" ]

                Logout ->
                    [ "logout" ]

                Upload ->
                    [ "upload" ]

                Portfolio ->
                    [ "portfolio" ]

                Reports ->
                    [ "reports" ]

                AccountBalance ->
                    [ "account-balance" ]

                ApiDocument ->
                    [ "api-document" ]
    in
    "#/" ++ String.join "/" pieces
