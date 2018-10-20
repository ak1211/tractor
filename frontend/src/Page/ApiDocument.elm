module Page.ApiDocument exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

import Api.Endpoint as Endpoint
import Bulma.Components as Components exposing (messageModifiers)
import Bulma.Layout as Layout
import Html exposing (Html)
import Html.Attributes as Attrib
import Http
import Markdown
import Session exposing (Session)


getWebApiDocument : Cmd Msg
getWebApiDocument =
    Http.send UpdateWebApiDocument (Http.getString "public/WebApiDocument.md")



-- MODEL


type alias Model =
    { session : Session
    , isMenuOpen : Bool
    , webApiDocument : Maybe String
    }


init : Session -> ( Model, Cmd Msg )
init session =
    let
        model =
            { session = session
            , isMenuOpen = False
            , webApiDocument = Nothing
            }
    in
    ( model, getWebApiDocument )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "API document"
    , content =
        case model.webApiDocument of
            Nothing ->
                Html.p [] []

            Just doc ->
                Layout.section Layout.NotSpaced
                    []
                    [ Layout.container []
                        [ Markdown.toHtml [ Attrib.class "content" ] doc
                        ]
                    ]
    }



-- UPDATE


type Msg
    = NoOp
    | GotSession Session
    | ToggleMenuOpen
    | UpdateWebApiDocument (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotSession session ->
            ( { model | session = session }, Cmd.none )

        ToggleMenuOpen ->
            ( { model | isMenuOpen = not model.isMenuOpen }, Cmd.none )

        UpdateWebApiDocument result ->
            ( { model | webApiDocument = Result.toMaybe result }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
