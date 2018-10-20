module Page.Dashboard exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

import Api.Endpoint as Endpoint
import Browser
import Browser.Navigation as Navigation
import Bulma.Components as Components exposing (messageModifiers)
import Bulma.Elements as Elements exposing (tagModifiers)
import Bulma.Form as Form
import Bulma.Layout as Layout
import Bulma.Modifiers as Modifiers
import Bulma.Modifiers.Typography as Typo
import Html exposing (Html)
import Html.Attributes as Attrib
import Http
import Json.Decode
import Session exposing (Session)
import Time
import Tuple
import Url
import Username
import Viewer



-- MODEL


type alias Model =
    { session : Session
    , isMenuOpen : Bool
    , systemHealth : Maybe (Result Http.Error Endpoint.SystemHealth)
    , systemVersion : Maybe Endpoint.VerRev
    }


init : Session -> ( Model, Cmd Msg )
init session =
    let
        model =
            { session = session
            , isMenuOpen = False
            , systemHealth = Nothing
            , systemVersion = Nothing
            }
    in
    ( model, Cmd.batch [ getSystemHealth, getSystemVersion ] )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Dashboard"
    , content =
        Layout.section Layout.NotSpaced
            []
            [ Layout.container [] <| viewDashboard model ]
    }


viewDashboard : Model -> List (Html Msg)
viewDashboard model =
    [ viewStatusLine model
    , viewGreetings model
    ]


viewStatusLine : Model -> Html Msg
viewStatusLine model =
    let
        control =
            Form.control Form.controlModifiers []

        greenBadge =
            control [ badge ( Modifiers.Dark, Modifiers.Success ) ( "System", "green" ) ]

        warningBadge =
            control [ badge ( Modifiers.Dark, Modifiers.Warning ) ( "System", "warning" ) ]

        failureBadge =
            control [ badge ( Modifiers.Dark, Modifiers.Danger ) ( "System", "failure" ) ]

        haltedBadge =
            control [ badge ( Modifiers.Dark, Modifiers.Danger ) ( "System", "halted" ) ]

        healthBadge : Result Http.Error Endpoint.SystemHealth -> Form.Control Msg
        healthBadge =
            Result.withDefault haltedBadge
                << Result.map
                    (\x ->
                        case x.system of
                            "Green" ->
                                greenBadge

                            "Yellow" ->
                                warningBadge

                            _ ->
                                failureBadge
                    )

        infoBadge =
            badge ( Modifiers.Dark, Modifiers.Info )

        versionBadge v =
            infoBadge ( "Version", v.version )

        archBadge v =
            infoBadge ( "Arch", v.buildArch )

        osBadge v =
            infoBadge ( "OS", v.buildOS )

        gitReposBadge v =
            infoBadge ( "Repos", v.gitStatus )

        gitCommitsBadge v =
            infoBadge ( "Commits", v.gitCommitCount )

        vtags maybeSV =
            case maybeSV of
                Nothing ->
                    []

                Just sv ->
                    [ control [ versionBadge sv ]
                    , control [ archBadge sv ]
                    , control [ osBadge sv ]
                    , control [ gitReposBadge sv ]
                    , control [ gitCommitsBadge sv ]
                    ]

        maybeToList m =
            case m of
                Nothing ->
                    []

                Just x ->
                    [ x ]
    in
    Form.multilineFields [] <|
        List.concat
            [ List.map healthBadge (maybeToList model.systemHealth)
            , vtags model.systemVersion
            ]


badge : ( Modifiers.Color, Modifiers.Color ) -> ( String, String ) -> Elements.Tag Msg
badge colors texts =
    let
        tag col txt =
            Elements.tag { tagModifiers | color = col } [] [ Html.text txt ]
    in
    Elements.multitag []
        [ tag (Tuple.first colors) (Tuple.first texts)
        , tag (Tuple.second colors) (Tuple.second texts)
        ]


viewGreetings : Model -> Elements.Notification Msg
viewGreetings model =
    let
        username =
            Session.viewer model.session
                |> Maybe.map Viewer.username
                |> Maybe.map Username.toString
                |> Maybe.withDefault "Guest"

        message =
            Html.p [ Typo.textSize Typo.Medium ]
                [ Html.text "Maido!"
                , Html.span
                    [ Attrib.style "margin" "0 1.8rem" ]
                    [ Html.text username ]
                , Html.text "san."
                ]
    in
    Elements.notification Modifiers.Dark [ Typo.textCentered ] [ message ]



-- UPDATE


type Msg
    = NoOp
    | GotSession Session
    | ToggleMenuOpen
    | GotSystemHealth (Result Http.Error Endpoint.SystemHealth)
    | GotSystemVersion (Result Http.Error Endpoint.VerRev)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotSession session ->
            ( { model | session = session }, Cmd.none )

        ToggleMenuOpen ->
            ( { model | isMenuOpen = not model.isMenuOpen }, Cmd.none )

        GotSystemHealth result ->
            ( { model | systemHealth = Just result }, Cmd.none )

        GotSystemVersion result ->
            ( { model | systemVersion = Result.toMaybe result }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- INTERNAL


getSystemHealth : Cmd Msg
getSystemHealth =
    Http.send GotSystemHealth Endpoint.getApiV1Health


getSystemVersion : Cmd Msg
getSystemVersion =
    Http.send GotSystemVersion Endpoint.getApiV1Version



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
