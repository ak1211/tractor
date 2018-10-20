{-
   https://github.com/rtfeldman/elm-spa-example

   Copyright (c) 2017-2018 Richard Feldman and contributors
   see https://github.com/rtfeldman/elm-spa-example/blob/master/LICENSE

   Changes since Oct. 2018 copyright &copy; by Akihiro Yamamoto
   see https://github.com/ak1211/tractor/blob/master/LICENSE
-}


port module Api exposing (Cred, application, credFromJWT, logout, storeCredWith, username, viewerChanges)

import Api.Endpoint as Endpoint exposing (JWT)
import Browser
import Browser.Navigation as Navigation
import Http
import Json.Decode exposing (Decoder, decodeString)
import Json.Decode.Pipeline exposing (required)
import Json.Encode
import Jwt exposing (JwtError)
import Task
import Url exposing (Url)
import Username exposing (Username(..))



-- CRED


{-| The authentication credentials for the Viewer (that is, the currently logged-in user.)
This includes:

  - The cred's authentication token
    By design, there is no way to access the token directly as a String.
    It can be encoded for persistence, and it can be added to a header
    to a HttpBuilder for a request, but that's it.
    This token should never be rendered to the end user, and with this API, it
    can't be!

-}
type Cred
    = Cred Username JWT


username : Cred -> Username
username (Cred val _) =
    val



--    authenticatedUser jwt
--        |> Result.map (\a -> Username a.userRealName)
--        |> Result.withDefault (Username "ERROR")


credHeader : Cred -> Http.Header
credHeader (Cred _ str) =
    Http.header "authorization" ("Bearer " ++ str)


{-| It's important that this is never exposed!

We epxose `login` and `application` instead, so we can be certain that if anyone
ever has access to a `Cred` value, it came from either the login API endpoint
or was passed in via flags.

-}
credDecoder : Decoder Cred
credDecoder =
    Json.Decode.succeed Cred
        |> required "username" Username.decoder
        |> required "token" Json.Decode.string



-- PERSISTENCE


decode : Json.Decode.Value -> Result Json.Decode.Error Cred
decode value =
    -- It's stored in localStorage as a JSON String;
    -- first decode the Value as a String, then
    -- decode that String as JSON.
    Json.Decode.decodeValue Json.Decode.string value
        |> Result.andThen (\str -> decodeString (Json.Decode.field "user" credDecoder) str)


port onStoreChange : (Json.Decode.Value -> msg) -> Sub msg


viewerChanges : (Maybe viewer -> msg) -> Decoder (Cred -> viewer) -> Sub msg
viewerChanges toMsg decoder =
    onStoreChange (\value -> toMsg (decodeFromChange decoder value))


decodeFromChange : Decoder (Cred -> viewer) -> Json.Decode.Value -> Maybe viewer
decodeFromChange viewerDecoder val =
    -- It's stored in localStorage as a JSON String;
    -- first decode the Value as a String, then
    -- decode that String as JSON.
    Json.Decode.decodeValue (storageDecoder viewerDecoder) val
        |> Result.toMaybe


storeCredWith : Cred -> Cmd msg
storeCredWith (Cred uname token) =
    let
        json =
            Json.Encode.object
                [ ( "user"
                  , Json.Encode.object
                        [ ( "username", Username.encode uname )
                        , ( "token", Json.Encode.string token )
                        ]
                  )
                ]
    in
    storeCache (Just json)


logout : Cmd msg
logout =
    storeCache Nothing


port storeCache : Maybe Json.Encode.Value -> Cmd msg


credFromJWT : JWT -> Result JwtError Cred
credFromJWT jwt =
    authenticatedUser jwt
        |> Result.map (\x -> Cred (Username x.userRealName) jwt)



{-
   case result of
       Ok a ->
           storeCredWith (Cred a jwt)

       Err b ->
           msg result
-}
-- SERIALIZATION
-- APPLICATION


application :
    Decoder (Cred -> viewer)
    ->
        { init : Maybe viewer -> Url -> Navigation.Key -> ( model, Cmd msg )
        , onUrlChange : Url -> msg
        , onUrlRequest : Browser.UrlRequest -> msg
        , subscriptions : model -> Sub msg
        , update : msg -> model -> ( model, Cmd msg )
        , view : model -> Browser.Document msg
        }
    -> Program Json.Decode.Value model msg
application viewerDecoder config =
    let
        init : Json.Decode.Value -> Url -> Navigation.Key -> ( model, Cmd msg )
        init flags url navKey =
            let
                maybeViewer =
                    Json.Decode.decodeValue Json.Decode.string flags
                        |> Result.andThen (decodeString (storageDecoder viewerDecoder))
                        |> Result.toMaybe
            in
            config.init maybeViewer url navKey
    in
    Browser.application
        { init = init
        , onUrlChange = config.onUrlChange
        , onUrlRequest = config.onUrlRequest
        , subscriptions = config.subscriptions
        , update = config.update
        , view = config.view
        }


storageDecoder : Decoder (Cred -> viewer) -> Decoder viewer
storageDecoder viewerDecoder =
    Json.Decode.field "user" (decoderFromCred viewerDecoder)


decoderFromCred : Decoder (Cred -> a) -> Decoder a
decoderFromCred decoder =
    Json.Decode.map2 identity decoder credDecoder



-- INTERNAL


type alias JWTContents =
    { dat : Endpoint.AuthenticatedUser
    }


decodeJWTContents : Decoder JWTContents
decodeJWTContents =
    Json.Decode.succeed JWTContents
        |> required "dat" Endpoint.decodeAuthenticatedUser


authenticatedUser : Endpoint.JWT -> Result JwtError Endpoint.AuthenticatedUser
authenticatedUser jwt =
    Jwt.decodeToken decodeJWTContents jwt
        |> Result.map (\a -> a.dat)
