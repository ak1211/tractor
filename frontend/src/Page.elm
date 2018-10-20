{-
   https://github.com/rtfeldman/elm-spa-example

   Copyright (c) 2017-2018 Richard Feldman and contributors
   see https://github.com/rtfeldman/elm-spa-example/blob/master/LICENSE

   Changes since Oct. 2018 copyright &copy; by Akihiro Yamamoto
   see https://github.com/ak1211/tractor/blob/master/LICENSE
-}


module Page exposing (view, viewErrors)

import Api.Endpoint as Endpoint
import Browser exposing (Document)
import Bulma.Components as Components
import Bulma.Elements as Elements
import Bulma.Form as Form
import Bulma.Layout as Layout
import Bulma.Modifiers as Modifiers
import Bulma.Modifiers.Typography as Typo
import Html exposing (Html)
import Html.Attributes as Attrib
import Html.Events exposing (onClick)
import Json.Decode
import Route exposing (Route)
import Session exposing (Session)
import Viewer exposing (Viewer)


{-| Take a page's Html and frames it with a header and footer.
The caller provides the current user, so we can display in either
"signed in" (rendering username) or "signed out" mode.
isLoading is for determining whether we should show a loading spinner
in the header. (This comes up during slow page transitions.)
-}
view : Bool -> msg -> Maybe Viewer -> { title : String, content : Html msg } -> Document msg
view isMenuOpen toggle maybeViewer { title, content } =
    { title = title ++ " - TRACTOR"
    , body =
        [ viewNavbar isMenuOpen toggle maybeViewer
        , viewHero title
        , content
        , viewFooter
        ]
    }


viewNavbar : Bool -> msg -> Maybe Viewer -> Html msg
viewNavbar isMenuOpen toggle maybeViewer =
    let
        modi =
            Components.navbarModifiers

        navbarBrand =
            Components.navbarBrand []
                (Components.navbarBurger isMenuOpen
                    []
                    [ Html.div
                        [ stopPropagationOnClick toggle ]
                        [ Html.span [] []
                        , Html.span [] []
                        , Html.span [] []
                        , Html.span [] []
                        , Html.span [] []
                        , Html.span [] []
                        , Html.span [] []
                        ]
                    ]
                )
                [ Components.navbarItemLink False
                    [ Route.href Route.Dashboard
                    , Typo.textSize Typo.Medium
                    , Typo.textColor Typo.BlackLight
                    , Attrib.style "font-family" "'Gugi', cursive"
                    ]
                    [ Html.text "TRACTOR" ]
                ]

        navbarMenu =
            Components.navbarMenu isMenuOpen [] [ navbarStart, navbarEnd ]

        navbarStart =
            Components.navbarStart []
                [ Components.navbarItemLink False [ Route.href Route.Dashboard ] [ Html.text "Dashboard" ]
                , Components.navbarItemLink False [ Route.href Route.Upload ] [ Html.text "Upload" ]
                , Components.navbarItemLink False [ Route.href Route.Portfolio ] [ Html.text "Portfolio" ]
                , Components.navbarItemLink False [ Route.href Route.Reports ] [ Html.text "Reports" ]
                , Components.navbarItemLink False [ Route.href Route.AccountBalance ] [ Html.text "Account balance" ]
                , Components.navbarItemLink False [ Route.href Route.ApiDocument ] [ Html.text "API document" ]
                ]

        navbarEnd =
            Components.navbarEnd []
                [ Components.navbarItem False [] [ Form.fields Modifiers.Left [] [ alternateButton ] ] ]

        alternateButton =
            let
                ( route, button ) =
                    case maybeViewer of
                        Nothing ->
                            ( Route.Login Nothing, iconButton "fas fa-sign-in-alt" Modifiers.Warning "Log in" )

                        Just _ ->
                            ( Route.Logout, iconButton "fas fa-sign-out-alt" Modifiers.Dark "Log out" )
            in
            Components.navbarItemLink False [ Route.href route ] [ button ]

        iconButton iconName color caption =
            let
                icon =
                    Elements.icon Modifiers.Standard [] [ Html.i [ Attrib.class iconName ] [] ]

                button =
                    Elements.button (Elements.buttonModifiers |> (\x -> { x | color = color }))
            in
            button
                [ Typo.textWeight Typo.Bold ]
                [ icon, Html.span [] [ Html.text caption ] ]
    in
    Components.fixedNavbar Modifiers.Top
        modi
        [ Attrib.style "border-bottom" "solid thin gainsboro" ]
        [ navbarBrand, navbarMenu ]


stopPropagationOnClick message =
    Html.Events.stopPropagationOn "click" (Json.Decode.map alwaysStop (Json.Decode.succeed message))


alwaysStop : msg -> ( msg, Bool )
alwaysStop msg =
    ( msg, True )


viewHero : String -> Html msg
viewHero title =
    Layout.hero Layout.heroModifiers
        []
        [ Layout.heroBody []
            [ Layout.container []
                [ Elements.title Elements.H1
                    [ Attrib.style "border-left" "solid 7px burlywood"
                    , Attrib.style "padding" "0.8rem 1.2rem"
                    ]
                    [ Html.text title ]
                ]
            ]
        ]


viewFooter : Html msg
viewFooter =
    let
        copy =
            Html.p []
                [ Html.text "The Assets observation application "
                , Html.span
                    [ Typo.textSize Typo.Medium
                    , Typo.textColor Typo.BlackLight
                    , Attrib.style "font-family" "'Gugi', cursive"
                    ]
                    [ Html.text "TRACTOR" ]
                , Html.text "."
                ]

        forkme =
            Html.div [ Attrib.style "display" "inline-block" ]
                [ Html.strong [ Attrib.style "vertical-align" "middle" ]
                    [ Html.text "Fork me on " ]
                , Html.a
                    [ Attrib.href "https://github.com/ak1211/tractor" ]
                    [ Html.img
                        [ Attrib.style "margin" "0 10px"
                        , Attrib.style "vertical-align" "middle"
                        , Attrib.src "public/assets/GitHub-Mark-32px.png"
                        , Attrib.alt "GitHub Logo"
                        , Attrib.width 24
                        , Attrib.height 24
                        ]
                        []
                    ]
                ]

        bulma =
            Html.div [ Attrib.style "display" "inline-block" ]
                [ Html.a
                    [ Attrib.href "https://bulma.io" ]
                    [ Html.img
                        [ Attrib.src "public/assets/made-with-bulma.png"
                        , Attrib.alt "Made with Bulma"
                        , Attrib.width 128
                        , Attrib.height 24
                        ]
                        []
                    ]
                ]
    in
    Layout.footer []
        [ Layout.container []
            [ Elements.content Modifiers.Standard
                [ Typo.textCentered ]
                [ copy, forkme ]
            ]
        ]


{-| Render dismissable errors. We use this all over the place!
-}
viewErrors : List String -> Html msg
viewErrors errors =
    let
        notification =
            Elements.notification Modifiers.Danger
                []
            <|
                List.map (\x -> Html.p [] [ Html.text x ]) errors
    in
    Layout.tileAncestor Modifiers.Auto
        [ Typo.textSize Typo.Medium, Typo.textCentered ]
        [ Layout.verticalTile Modifiers.Auto [] []
        , Layout.verticalTile Modifiers.Width4 [] [ notification ]
        , Layout.verticalTile Modifiers.Auto [] []
        ]
