module Page.Upload exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

import Browser
import Browser.Navigation as Navigation
import Bulma.CDN exposing (stylesheet)
import Bulma.Columns as Columns exposing (ColumnModifiers, columnModifiers, columnsModifiers)
import Bulma.Components exposing (..)
import Bulma.Elements exposing (..)
import Bulma.Form exposing (..)
import Bulma.Layout as Layout exposing (heroModifiers)
import Bulma.Modifiers exposing (..)
import Bulma.Modifiers.Typography as Typo
import Html exposing (Html)
import Html.Attributes as Attrib
import Json.Decode
import Session exposing (Session)
import Url



-- MODEL


type alias Model =
    { session : Session
    , isMenuOpen : Bool
    }


init : Session -> ( Model, Cmd Msg )
init session =
    let
        model =
            { session = session, isMenuOpen = False }
    in
    ( model, Cmd.none )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Upload"
    , content =
        Html.main_ []
            [ exampleElementsAndComponents ]
    }


myColumnModifiers : Width -> Maybe Width -> ColumnModifiers
myColumnModifiers offset width =
    let
        widths : Devices (Maybe Width)
        widths =
            columnModifiers.widths
    in
    { columnModifiers
        | offset =
            offset
        , widths =
            { widths
                | tablet = width
                , desktop = width
                , widescreen = width
                , fullHD = width
            }
    }


demoArticle : String -> List (Html Msg) -> Html Msg
demoArticle aTitle someHtmls =
    Columns.columns columnsModifiers
        []
        [ Columns.column (myColumnModifiers Auto (Just Width2))
            []
            [ title H4 [] [ Html.strong [] [ Html.text aTitle ] ]
            ]
        , Columns.column (myColumnModifiers Auto (Just Auto))
            []
            someHtmls
        ]


demoSection : String -> List (Html.Attribute Msg) -> List (Html Msg) -> Html Msg
demoSection aSubtitle someAttrs someHtmls =
    Columns.columns columnsModifiers
        someAttrs
        [ Columns.column (myColumnModifiers Auto (Just Width3))
            []
            [ subtitle H4 [] [ Html.text aSubtitle ]
            ]
        , Columns.column (myColumnModifiers Auto (Just Auto))
            []
            someHtmls
        ]


exampleMediaObject : Html Msg
exampleMediaObject =
    Layout.media []
        [ Layout.mediaLeft []
            [ image (OneByOne X64)
                []
                [ Html.img [ Attrib.src "https://bulma.io/images/placeholders/128x128.png" ] []
                ]
            ]
        , Layout.mediaContent []
            [ content Standard
                []
                [ Html.p []
                    [ Html.strong [] [ Html.text "John Smith" ]
                    , Html.text " "
                    , Html.small [] [ Html.text "@johnsmith" ]
                    , Html.text " "
                    , Html.small [] [ Html.text "31m" ]
                    , Html.br [] []
                    , Html.text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean efficitur sit amet massa fringilla egestas. Nullam condimentum luctus turpis."
                    ]
                ]
            , Layout.horizontalLevel []
                [ Layout.levelLeft []
                    [ Layout.levelItemLink [] [ icon Small [] [ Html.i [ Attrib.class "fa fa-reply" ] [] ] ]
                    , Layout.levelItemLink [] [ icon Small [] [ Html.i [ Attrib.class "fa fa-retweet" ] [] ] ]
                    , Layout.levelItemLink [] [ icon Small [] [ Html.i [ Attrib.class "fa fa-heart" ] [] ] ]
                    ]
                ]
            ]
        ]


exampleElementsAndComponents : Html Msg
exampleElementsAndComponents =
    Layout.section Layout.NotSpaced
        []
        [ Layout.container []
            [ demoArticle "Elements"
                [ demoSection "Form"
                    []
                    [ field []
                        [ controlLabel [] [ Html.text "Form Label" ]
                        , controlInput controlInputModifiers [] [ Attrib.placeholder "Input" ] []
                        , controlHelp Default [] [ Html.text "This field isn't required!" ]
                        ]
                    , field []
                        [ controlSelect controlSelectModifiers
                            []
                            []
                            [ Html.option [] [ Html.text "Dropdown" ]
                            ]
                        ]
                    , field []
                        [ controlTextArea controlTextAreaModifiers [] [ Attrib.placeholder "Textarea" ] []
                        ]
                    , field []
                        [ controlCheckBox False
                            []
                            []
                            []
                            [ Html.text "Checkbox"
                            ]
                        ]
                    , field []
                        [ controlRadio []
                            [ controlRadioButton False
                                True
                                "yes"
                                []
                                []
                                [ Html.text "Radio"
                                ]
                            , controlRadioButton False
                                False
                                "no"
                                []
                                []
                                [ Html.text "Button"
                                ]
                            ]
                        ]
                    , field []
                        [ controlButton { buttonModifiers | color = Link }
                            []
                            []
                            [ Html.text "Button"
                            ]
                        ]
                    ]
                , demoSection "Box"
                    []
                    [ box []
                        [ exampleMediaObject
                        ]
                    ]
                , demoSection "Button"
                    []
                    [ buttons Left [] <|
                        List.map (\( color, label ) -> button { buttonModifiers | color = color } [] [ Html.text label ])
                            [ ( Default, "Default" )
                            , ( White, "White" )
                            , ( Light, "Light" )
                            , ( Dark, "Dark" )
                            , ( Black, "Black" )
                            ]
                    , buttons Left [] <|
                        List.map (\( color, label ) -> button { buttonModifiers | color = color } [] [ Html.text label ])
                            [ ( Primary, "Primary" )
                            , ( Link, "Link" )
                            , ( Info, "Info" )
                            , ( Success, "Success" )
                            , ( Warning, "Warning" )
                            , ( Danger, "Danger" )
                            ]
                    ]
                , demoSection "Notification"
                    []
                    [ notificationWithDelete Primary
                        []
                        NoOp
                        [ Html.text "Lorem ipsum dolor sit amet, consectetur adipiscing elit lorem ipsum dolor. Pellentesque risus mi, tempus quis placerat ut, porta nec nulla. Vestibulum rhoncus ac ex sit amet fringilla. Nullam gravida purus diam, et dictum felis venenatis efficitur. Sit amet, consectetur adipiscing elit"
                        ]
                    ]
                , demoSection "Progress Bar"
                    []
                    [ easyProgress { progressModifiers | color = Primary } [] 0.4
                    ]
                , demoSection "Tags"
                    []
                    [ tags [] <|
                        List.map (\( color, label ) -> tag { tagModifiers | color = color } [] [ Html.text label ])
                            [ ( Primary, "Primary" )
                            , ( Link, "Link" )
                            , ( Info, "Info" )
                            , ( Success, "Success" )
                            , ( Warning, "Warning" )
                            , ( Danger, "Danger" )
                            ]
                    , tags [] <|
                        List.map (\( color, label ) -> tag { tagModifiers | color = color } [] [ Html.text label ])
                            [ ( Default, "Default" )
                            , ( White, "White" )
                            , ( Light, "Light" )
                            , ( Dark, "Dark" )
                            , ( Black, "Black" )
                            ]
                    ]
                ]
            , demoArticle "Components"
                [ demoSection "Card"
                    []
                    [ Columns.columns columnsModifiers
                        []
                        [ Columns.column columnModifiers
                            []
                            [ card []
                                [ cardImage []
                                    [ image FourByThree
                                        []
                                        [ Html.img [ Attrib.src "https://bulma.io/images/placeholders/1280x960.png" ] []
                                        ]
                                    ]
                                , cardContent []
                                    [ Layout.media []
                                        [ Layout.mediaLeft []
                                            [ image (OneByOne X48)
                                                []
                                                [ Html.img [ Attrib.src "https://bulma.io/images/placeholders/96x96.png" ] []
                                                ]
                                            ]
                                        , Layout.mediaContent [] <|
                                            easyTitleWithSubtitle False
                                                H4
                                                [ Html.text "John Smith" ]
                                                [ Html.text "@johnsmith" ]
                                        ]
                                    , content Standard
                                        []
                                        [ Html.text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Phasellus nec iaculis mauris."
                                        , Html.a [] [ Html.text "@bulmaio" ]
                                        , Html.text "."
                                        , Html.a [] [ Html.text "#css" ]
                                        , Html.text " "
                                        , Html.a [] [ Html.text "#responsive" ]
                                        , Html.small [] [ Html.text "11:09 PM - 1 Jan 2016" ]
                                        ]
                                    ]
                                ]
                            ]
                        , Columns.column columnModifiers
                            []
                            [ card []
                                [ cardContent [] <|
                                    easyTitleWithSubtitle False
                                        H3
                                        [ Html.text "“There are two hard things in computer science: cache invalidation, naming things, and off-by-one errors.”" ]
                                        [ Html.text "Jeff Atwood" ]
                                , cardFooter []
                                    [ cardFooterItem []
                                        [ Html.span []
                                            [ Html.text "View on "
                                            , Html.a [] [ Html.text "Twitter" ]
                                            ]
                                        ]
                                    , cardFooterItem []
                                        [ Html.span []
                                            [ Html.text "Share on "
                                            , Html.a [] [ Html.text "Facebook" ]
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                , demoSection "Dropdown"
                    [ Attrib.style "height" "16rem" ]
                    [ dropdown True
                        dropdownModifiers
                        []
                        [ dropdownTrigger []
                            [ button buttonModifiers
                                [ Attrib.attribute "aria-haspopup" "true"
                                , Attrib.attribute "aria-controls" "dropdown-menu"
                                ]
                                [ Html.text "Dropdown button"
                                ]
                            ]
                        , dropdownMenu []
                            []
                            [ dropdownItem False [] [ Html.text "Dropdown item" ]
                            , dropdownItemLink False [] [ Html.text "Other dropdown item" ]
                            , dropdownItemLink True [] [ Html.text "Active dropdown item" ]
                            , dropdownItemLink False [] [ Html.text "Other item" ]
                            , dropdownDivider [] []
                            , dropdownItemLink False [] [ Html.text "With a divider" ]
                            ]
                        ]
                    ]
                , demoSection "Message"
                    []
                    [ message { messageModifiers | color = Primary }
                        []
                        [ messageHeaderWithDelete [] NoOp [ Html.text "Primary" ]
                        , messageBody [] [ Html.text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Pellentesque risus mi, tempus quis placerat ut, porta nec nulla. Vestibulum rhoncus ac ex sit amet fringilla. Nullam gravida purus diam, et dictum felis venenatis efficitur. Aenean ac eleifend lacus, in mollis lectus. Donec sodales, arcu et sollicitudin porttitor, tortor urna tempor ligula, id porttitor mi magna a neque. Donec dui urna, vehicula et sem eget, facilisis sodales sem." ]
                        ]
                    ]
                , demoSection "Pagination"
                    []
                    [ pagination Left
                        []
                        [ paginationPrev [] [ Html.text "Previous" ]
                        , paginationNext [] [ Html.text "Next" ]
                        , paginationList []
                            [ paginationLink False [] [ Html.text "1" ]
                            , paginationEllipsis [] [ Html.text "…" ]
                            , paginationLink False [] [ Html.text "45" ]
                            , paginationLink True [] [ Html.text "46" ]
                            , paginationLink False [] [ Html.text "47" ]
                            , paginationEllipsis [] [ Html.text "…" ]
                            , paginationLink False [] [ Html.text "83" ]
                            ]
                        ]
                    ]
                , demoSection "Tabs"
                    []
                    [ tabs { tabsModifiers | style = Boxed }
                        []
                        []
                        [ tab True [] [] [ icon Standard [] [ Html.i [ Attrib.class "fa fa-image" ] [] ], Html.text "Pictures" ]
                        , tab False [] [] [ icon Standard [] [ Html.i [ Attrib.class "fa fa-music" ] [] ], Html.text "Music" ]
                        , tab False [] [] [ icon Standard [] [ Html.i [ Attrib.class "fa fa-film" ] [] ], Html.text "Videos" ]
                        , tab False [] [] [ icon Standard [] [ Html.i [ Attrib.class "fa fa-file-pdf-o" ] [] ], Html.text "Documents" ]
                        ]
                    ]
                , demoSection "Media Object"
                    []
                    [ exampleMediaObject
                    ]
                , demoSection "Menu & Panel"
                    []
                    [ Columns.columns columnsModifiers
                        []
                        [ Columns.column columnModifiers
                            []
                            [ menu []
                                [ menuLabel [] [ Html.text "General" ]
                                , menuList []
                                    [ menuListItem [] [ menuListItemLink False [] [ Html.text "Dashboard" ] ]
                                    , menuListItem [] [ menuListItemLink False [] [ Html.text "Customers" ] ]
                                    ]
                                , menuLabel [] [ Html.text "Administration" ]
                                , menuList []
                                    [ menuListItem []
                                        [ menuListItemLink False [] [ Html.text "Team Settings" ]
                                        ]
                                    , menuListItem []
                                        [ menuListItemLink True [] [ Html.text "Manage Your Team" ]
                                        , menuList []
                                            [ menuListItemLink False [] [ Html.text "Members" ]
                                            , menuListItemLink False [] [ Html.text "Plugins" ]
                                            , menuListItemLink False [] [ Html.text "Add a member" ]
                                            ]
                                        ]
                                    , menuListItem []
                                        [ menuListItemLink False [] [ Html.text "Invitations" ]
                                        ]
                                    , menuListItem []
                                        [ menuListItemLink False [] [ Html.text "Cloud Storage Environment Settings" ]
                                        ]
                                    , menuListItem []
                                        [ menuListItemLink False [] [ Html.text "Authentication" ]
                                        ]
                                    ]
                                , menuLabel [] [ Html.text "Transactions" ]
                                , menuList []
                                    [ menuListItem [] [ menuListItemLink False [] [ Html.text "Payments" ] ]
                                    , menuListItem [] [ menuListItemLink False [] [ Html.text "Transfers" ] ]
                                    , menuListItem [] [ menuListItemLink False [] [ Html.text "Balance" ] ]
                                    ]
                                ]
                            ]
                        , Columns.column columnModifiers
                            []
                            [ panel []
                                [ panelHeading [] [ Html.text "Repositories" ]
                                , panelBlock False
                                    []
                                    [ controlInput controlInputModifiers [] [] []
                                    ]
                                , panelTabs []
                                    [ panelTab False [] [ Html.text "All" ]
                                    , panelTab True [] [ Html.text "Public" ]
                                    , panelTab True [] [ Html.text "Private" ]
                                    , panelTab True [] [ Html.text "Sources" ]
                                    , panelTab True [] [ Html.text "Forks" ]
                                    ]
                                , panelLinkWithIcon True [] [] [ Html.i [ Attrib.class "fa fa-book" ] [] ] [ Html.text "bulma" ]
                                , panelLinkWithIcon False [] [] [ Html.i [ Attrib.class "fa fa-book" ] [] ] [ Html.text "marksheet" ]
                                , panelLinkWithIcon False [] [] [ Html.i [ Attrib.class "fa fa-book" ] [] ] [ Html.text "minireset.css" ]
                                , panelLinkWithIcon False [] [] [ Html.i [ Attrib.class "fa fa-book" ] [] ] [ Html.text "jgthms.github.io" ]
                                , panelLinkWithIcon False [] [] [ Html.i [ Attrib.class "fa fa-code-fork" ] [] ] [ Html.text "daniellowtw/infBoard" ]
                                , panelLinkWithIcon False [] [] [ Html.i [ Attrib.class "fa fa-code-fork" ] [] ] [ Html.text "mojs" ]
                                , panelCheckbox False [] [] [ Html.text "Remember Me" ]
                                , panelBlock False
                                    []
                                    [ button { buttonModifiers | color = Link }
                                        [ fullWidth ]
                                        [ Html.text "Reset all filters"
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]



-- UPDATE


type Msg
    = NoOp
    | GotSession Session
    | ToggleMenuOpen


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotSession session ->
            ( { model | session = session }, Cmd.none )

        ToggleMenuOpen ->
            ( { model | isMenuOpen = not model.isMenuOpen }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
