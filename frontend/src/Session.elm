module Session exposing (Session, changes, cred, fromViewer, navKey, viewer)

import Api exposing (Cred)
import Browser.Navigation as Navigation
import Json.Decode
import Json.Decode.Pipeline
import Viewer exposing (Viewer)



-- TYPES


type Session
    = LoggedIn Navigation.Key Viewer
    | Guest Navigation.Key



-- INFO


viewer : Session -> Maybe Viewer
viewer session =
    case session of
        LoggedIn _ val ->
            Just val

        Guest _ ->
            Nothing


cred : Session -> Maybe Cred
cred session =
    case session of
        LoggedIn _ val ->
            Just (Viewer.cred val)

        Guest _ ->
            Nothing


navKey : Session -> Navigation.Key
navKey session =
    case session of
        LoggedIn key _ ->
            key

        Guest key ->
            key



-- CHANGES


changes : (Session -> msg) -> Navigation.Key -> Sub msg
changes toMsg key =
    Api.viewerChanges (\maybeViewer -> toMsg (fromViewer key maybeViewer)) Viewer.decoder


fromViewer : Navigation.Key -> Maybe Viewer -> Session
fromViewer key maybeViewer =
    case maybeViewer of
        -- It's stored in localStorage as a JSON String;
        -- first decode the Value as a String, then
        -- decode that String as JSON.
        Just viewerVal ->
            LoggedIn key viewerVal

        Nothing ->
            Guest key
