{-
   https://github.com/rtfeldman/elm-spa-example

   Copyright (c) 2017-2018 Richard Feldman and contributors
   see https://github.com/rtfeldman/elm-spa-example/blob/master/LICENSE

   Changes since Oct. 2018 copyright &copy; by Akihiro Yamamoto
   see https://github.com/ak1211/tractor/blob/master/LICENSE
-}


module Main exposing (main)

import Api
import Api.Endpoint as Endpoint
import Browser
import Browser.Navigation as Navigation
import Html exposing (Html)
import Html.Attributes as Attrib
import Http
import Json.Decode
import Jwt exposing (JwtError)
import Page
import Page.AccountBalance as AccBalance
import Page.ApiDocument as ApiDocument
import Page.Blank as Blank
import Page.Dashboard as Dashboard
import Page.Login as Login
import Page.NotFound as NotFound
import Page.Portfolio as Portfolio
import Page.Reports as Reports
import Page.Upload as Upload
import Route exposing (Route)
import Session exposing (Session)
import Url exposing (Url)
import Viewer exposing (Viewer)



-- MODEL


type Model
    = Redirect Session
    | NotFound Session
    | Dashboard Dashboard.Model
    | Upload Upload.Model
    | Portfolio Portfolio.Model
    | Reports Reports.Model
    | AccountBalance AccBalance.Model
    | Login Login.Model
    | ApiDocument ApiDocument.Model


init : Maybe Viewer -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init maybeViewer url navKey =
    changeRouteTo (Route.fromUrl url)
        (Redirect (Session.fromViewer navKey maybeViewer))



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        viewPage isMenuOpen toggleMsg toMsg config =
            let
                { title, body } =
                    Page.view isMenuOpen toggleMsg (Session.viewer (toSession model)) config
            in
            { title = title
            , body = List.map (Html.map toMsg) body
            }
    in
    case model of
        Redirect _ ->
            viewPage False Ignored (always Ignored) Blank.view

        NotFound _ ->
            viewPage False Ignored (always Ignored) NotFound.view

        Dashboard dashboard ->
            Dashboard.view dashboard
                |> viewPage dashboard.isMenuOpen Dashboard.ToggleMenuOpen GotDashboardMsg

        Upload upload ->
            Upload.view upload
                |> viewPage upload.isMenuOpen Upload.ToggleMenuOpen GotUploadMsg

        Portfolio portfolio ->
            Portfolio.view portfolio
                |> viewPage portfolio.isMenuOpen Portfolio.ToggleMenuOpen GotPortfolioMsg

        Reports reports ->
            Reports.view reports
                |> viewPage reports.isMenuOpen Reports.ToggleMenuOpen GotReportsMsg

        AccountBalance accbalance ->
            AccBalance.view accbalance
                |> viewPage accbalance.isMenuOpen AccBalance.ToggleMenuOpen GotAccountBalanceMsg

        Login login ->
            Login.view login
                |> viewPage login.isMenuOpen Login.ToggleMenuOpen GotLoginMsg

        ApiDocument apidoc ->
            ApiDocument.view apidoc
                |> viewPage apidoc.isMenuOpen ApiDocument.ToggleMenuOpen GotApiDocumentMsg



-- UPDATE


type Msg
    = Ignored
    | ChangedRoute (Maybe Route)
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotDashboardMsg Dashboard.Msg
    | GotUploadMsg Upload.Msg
    | GotPortfolioMsg Portfolio.Msg
    | GotReportsMsg Reports.Msg
    | GotAccountBalanceMsg AccBalance.Msg
    | GotLoginMsg Login.Msg
    | GotApiDocumentMsg ApiDocument.Msg
    | GotSession Session


toSession : Model -> Session
toSession page =
    case page of
        Redirect session ->
            session

        NotFound session ->
            session

        Dashboard dashboard ->
            Dashboard.toSession dashboard

        Upload upload ->
            Upload.toSession upload

        Portfolio portfolio ->
            Portfolio.toSession portfolio

        Reports reports ->
            Reports.toSession reports

        AccountBalance accbalance ->
            AccBalance.toSession accbalance

        Login login ->
            Login.toSession login

        ApiDocument login ->
            ApiDocument.toSession login


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
    in
    case maybeRoute of
        Nothing ->
            ( NotFound session, Cmd.none )

        Just Route.Root ->
            ( Redirect session
            , Route.replaceUrl (Session.navKey (toSession model)) Route.Dashboard
            )

        Just Route.Logout ->
            ( Redirect session
            , Cmd.batch
                [ Api.logout
                , Route.replaceUrl (Session.navKey (toSession model)) Route.Dashboard
                , Navigation.reload
                ]
            )

        Just Route.Dashboard ->
            Dashboard.init session
                |> updateWith Dashboard GotDashboardMsg model

        Just (Route.Login param) ->
            Login.init param session
                |> updateWith Login GotLoginMsg model

        Just Route.Upload ->
            Upload.init session
                |> updateWith Upload GotUploadMsg model

        Just Route.Portfolio ->
            Portfolio.init session
                |> updateWith Portfolio GotPortfolioMsg model

        Just Route.Reports ->
            Reports.init session
                |> updateWith Reports GotReportsMsg model

        Just Route.AccountBalance ->
            AccBalance.init session
                |> updateWith AccountBalance GotAccountBalanceMsg model

        Just Route.ApiDocument ->
            ApiDocument.init session
                |> updateWith ApiDocument GotApiDocumentMsg model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Ignored, _ ) ->
            ( model, Cmd.none )

        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    case url.fragment of
                        Nothing ->
                            -- If we got a link that didn't include a fragment,
                            -- it's from one of those (href "") attributes that
                            -- we have to include to make the RealWorld CSS work.
                            --
                            -- In an application doing path routing instead of
                            -- fragment-based routing, this entire
                            -- `case url.fragment of` expression this comment
                            -- is inside would be unnecessary.
                            ( model, Cmd.none )

                        Just _ ->
                            ( model
                            , Navigation.pushUrl (Session.navKey (toSession model)) (Url.toString url)
                            )

                Browser.External href ->
                    ( model
                    , Navigation.load href
                    )

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( ChangedRoute route, _ ) ->
            changeRouteTo route model

        ( GotDashboardMsg subMsg, Dashboard dashboard ) ->
            Dashboard.update subMsg dashboard
                |> updateWith Dashboard GotDashboardMsg model

        ( GotLoginMsg subMsg, Login login ) ->
            Login.update subMsg login
                |> updateWith Login GotLoginMsg model

        ( GotUploadMsg subMsg, Upload upload ) ->
            Upload.update subMsg upload
                |> updateWith Upload GotUploadMsg model

        ( GotPortfolioMsg subMsg, Portfolio portfolio ) ->
            Portfolio.update subMsg portfolio
                |> updateWith Portfolio GotPortfolioMsg model

        ( GotReportsMsg subMsg, Reports reports ) ->
            Reports.update subMsg reports
                |> updateWith Reports GotReportsMsg model

        ( GotAccountBalanceMsg subMsg, AccountBalance accbalance ) ->
            AccBalance.update subMsg accbalance
                |> updateWith AccountBalance GotAccountBalanceMsg model

        ( GotApiDocumentMsg subMsg, ApiDocument apidoc ) ->
            ApiDocument.update subMsg apidoc
                |> updateWith ApiDocument GotApiDocumentMsg model

        ( GotSession session, Redirect _ ) ->
            ( Redirect session
            , Route.replaceUrl (Session.navKey session) Route.Dashboard
            )

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        NotFound _ ->
            Sub.none

        Redirect _ ->
            Session.changes GotSession (Session.navKey (toSession model))

        Dashboard dashboard ->
            Sub.map GotDashboardMsg (Dashboard.subscriptions dashboard)

        Login login ->
            Sub.map GotLoginMsg (Login.subscriptions login)

        Upload upload ->
            Sub.map GotUploadMsg (Upload.subscriptions upload)

        Portfolio portfolio ->
            Sub.map GotPortfolioMsg (Portfolio.subscriptions portfolio)

        Reports reports ->
            Sub.map GotReportsMsg (Reports.subscriptions reports)

        AccountBalance accbalance ->
            Sub.map GotAccountBalanceMsg (AccBalance.subscriptions accbalance)

        ApiDocument apidocument ->
            Sub.map GotApiDocumentMsg (ApiDocument.subscriptions apidocument)



-- MAIN


main : Program Json.Decode.Value Model Msg
main =
    Api.application Viewer.decoder
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }
