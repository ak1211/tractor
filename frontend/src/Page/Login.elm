module Page.Login exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

import Api
import Api.Endpoint as Endpoint exposing (AuthClientId)
import Browser
import Browser.Navigation as Navigation
import Bulma.Components as Components
import Bulma.Elements as Elements
import Bulma.Layout as Layout
import Bulma.Modifiers as Modifiers
import Bulma.Modifiers.Typography as Typo
import Html exposing (Html)
import Html.Attributes as Attrib
import Http
import Json.Decode
import Page
import Route exposing (AuthRedirectParam, AuthRedirectResult)
import Session exposing (Session)
import Url
import Url.Builder as Builder



-- MODEL


type alias Model =
    { session : Session
    , isMenuOpen : Bool
    , stage : Stage
    }


type Stage
    = First { authClientId : Maybe AuthClientId }
    | Redirected { authRedirectParam : AuthRedirectParam }
    | Error { cause : String }


init : Maybe AuthRedirectResult -> Session -> ( Model, Cmd Msg )
init result session =
    let
        initModel x =
            { session = session
            , isMenuOpen = False
            , stage = x
            }

        initFirstStage =
            First { authClientId = Nothing }

        initRedirectedStage param =
            Redirected { authRedirectParam = param }

        initErrorStage msg =
            Error { cause = msg }
    in
    case result of
        Nothing ->
            ( initFirstStage |> initModel, getAuthClientId )

        Just (Ok param) ->
            ( initRedirectedStage param |> initModel
            , getAccessToken param.code
            )

        Just (Err e) ->
            ( initErrorStage e |> initModel
            , Cmd.none
            )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    let
        concreteView =
            case model.stage of
                First stage ->
                    Maybe.map viewContents stage.authClientId
                        |> Maybe.withDefault [ Page.viewErrors NoOp [ "OAuth Flow ERROR" ] ]

                Redirected stage ->
                    -- Auth Success
                    [ Html.text "Success to Login" ]

                Error stage ->
                    [ Page.viewErrors NoOp [ "Auth error", stage.cause ] ]
    in
    { title = "Log in"
    , content =
        Layout.section Layout.NotSpaced
            []
            [ Layout.container [] concreteView ]
    }


viewContents : AuthClientId -> List (Html Msg)
viewContents authClientId =
    [ Html.p []
        [ signInWithSlackButton authClientId ]
    ]


signInWithSlackButton : AuthClientId -> Html Msg
signInWithSlackButton authClientId =
    let
        hrefSignInWithSlack =
            Builder.crossOrigin
                "https://slack.com"
                [ "oauth", "authorize" ]
                [ Builder.string "scope" "identity.basic"
                , Builder.string "client_id" authClientId.clientid
                , Builder.string "state" "abcdefg"
                ]

        modi =
            Elements.buttonModifiers
                |> (\x -> { x | color = Modifiers.Dark })

        icon =
            Elements.icon Modifiers.Standard [] [ Html.i [ Attrib.class "fab fa-slack" ] [] ]

        slackMark =
            Html.img
                [ Attrib.style "vertical-align" "middle"
                , Attrib.src "public/assets/Slack_Monochrome_White.svg"
                , Attrib.alt "Slack"
                , Attrib.width 80
                ]
                []
    in
    Html.p
        [ Typo.textCentered ]
        [ Html.a
            [ Attrib.href hrefSignInWithSlack ]
            [ Elements.button modi
                [ Typo.textSize Typo.Medium ]
                [ Html.div
                    [ Attrib.style "vertical-align" "middle"
                    , Attrib.style "display" "inline-block"
                    ]
                    [ slackMark
                    , Html.text "アカウントでログイン"
                    ]
                ]
            ]
        ]



-- UPDATE


type Msg
    = NoOp
    | ToggleMenuOpen
    | GotAuthClientId (Result Http.Error Endpoint.AuthClientId)
    | GotAccessToken (Result Http.Error Endpoint.RespAuth)
    | GotSession Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ToggleMenuOpen ->
            ( { model | isMenuOpen = not model.isMenuOpen }, Cmd.none )

        GotAccessToken result ->
            case result of
                Ok ok ->
                    let
                        store =
                            case Api.credFromJWT ok.accessToken of
                                Ok cred ->
                                    Api.storeCredWith cred

                                Err _ ->
                                    Cmd.none
                    in
                    ( model
                    , Cmd.batch [ store, Navigation.load "/" ]
                    )

                Err err ->
                    Debug.log "OAuth flow error"
                        ( model, Cmd.none )

        GotAuthClientId result ->
            case model.stage of
                First stage ->
                    let
                        newStage =
                            First { stage | authClientId = Result.toMaybe result }
                    in
                    ( { model | stage = newStage }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GotSession session ->
            ( { model | session = session }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- INTERNAL


getAuthClientId : Cmd Msg
getAuthClientId =
    Http.send GotAuthClientId Endpoint.getApiV1AuthClientid


getAccessToken : Endpoint.AuthTempCode -> Cmd Msg
getAccessToken code =
    Http.send GotAccessToken <| Endpoint.postApiV1Auth code



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
