module Page.Charts exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

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
    }


init : Session -> ( Model, Cmd Msg )
init session =
    let
        model =
            { session = session
            , isMenuOpen = False
            }
    in
    ( model, Cmd.none )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Charts"
    , content =
        Layout.section Layout.NotSpaced
            []
            [ Layout.container [] <| viewCharts model ]
    }


viewCharts : Model -> List (Html Msg)
viewCharts model =
    []



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



-- INTERNAL
-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
