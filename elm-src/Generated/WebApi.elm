module Generated.WebApi exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String


type alias AccessToken = String

type alias TimeFrame = String

type alias QueryLimit = Int

type alias MarketCode = String

type alias AuthzValue = String

makeAuthorizationHeader : AccessToken -> AuthzValue

makeAuthorizationHeader token = "Bearer " ++ token

type alias VerRev =
    { version : String
    , buildArch : String
    , buildOS : String
    , gitBranch : String
    , gitHash : String
    , gitCommitDate : String
    , gitCommitCount : String
    , gitStatus : String
    }

decodeVerRev : Decoder VerRev
decodeVerRev =
    decode VerRev
        |> required "version" string
        |> required "buildArch" string
        |> required "buildOS" string
        |> required "gitBranch" string
        |> required "gitHash" string
        |> required "gitCommitDate" string
        |> required "gitCommitCount" string
        |> required "gitStatus" string

encodeVerRev : VerRev -> Json.Encode.Value
encodeVerRev x =
    Json.Encode.object
        [ ( "version", Json.Encode.string x.version )
        , ( "buildArch", Json.Encode.string x.buildArch )
        , ( "buildOS", Json.Encode.string x.buildOS )
        , ( "gitBranch", Json.Encode.string x.gitBranch )
        , ( "gitHash", Json.Encode.string x.gitHash )
        , ( "gitCommitDate", Json.Encode.string x.gitCommitDate )
        , ( "gitCommitCount", Json.Encode.string x.gitCommitCount )
        , ( "gitStatus", Json.Encode.string x.gitStatus )
        ]

type alias AuthenticatedUser =
    { userId : String
    , userName : String
    , userRealName : String
    , userTz : String
    , userTzOffset : Int
    }

decodeAuthenticatedUser : Decoder AuthenticatedUser
decodeAuthenticatedUser =
    decode AuthenticatedUser
        |> required "userId" string
        |> required "userName" string
        |> required "userRealName" string
        |> required "userTz" string
        |> required "userTzOffset" int

encodeAuthenticatedUser : AuthenticatedUser -> Json.Encode.Value
encodeAuthenticatedUser x =
    Json.Encode.object
        [ ( "userId", Json.Encode.string x.userId )
        , ( "userName", Json.Encode.string x.userName )
        , ( "userRealName", Json.Encode.string x.userRealName )
        , ( "userTz", Json.Encode.string x.userTz )
        , ( "userTzOffset", Json.Encode.int x.userTzOffset )
        ]

type alias ApiAccessToken =
    { token : String
    }

decodeApiAccessToken : Decoder ApiAccessToken
decodeApiAccessToken =
    decode ApiAccessToken
        |> required "token" string

encodeApiAccessToken : ApiAccessToken -> Json.Encode.Value
encodeApiAccessToken x =
    Json.Encode.object
        [ ( "token", Json.Encode.string x.token )
        ]

type alias ApiPortfolio =
    { code : String
    , caption : Maybe (String)
    , updateAt : Maybe (String)
    }

decodeApiPortfolio : Decoder ApiPortfolio
decodeApiPortfolio =
    decode ApiPortfolio
        |> required "code" string
        |> required "caption" (maybe string)
        |> required "updateAt" (maybe string)

encodeApiPortfolio : ApiPortfolio -> Json.Encode.Value
encodeApiPortfolio x =
    Json.Encode.object
        [ ( "code", Json.Encode.string x.code )
        , ( "caption", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.caption )
        , ( "updateAt", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.updateAt )
        ]

type alias ApiOhlcv =
    { at : String
    , open : Maybe (Float)
    , high : Maybe (Float)
    , low : Maybe (Float)
    , close : Maybe (Float)
    , volume : Int
    , source : Maybe (String)
    }

decodeApiOhlcv : Decoder ApiOhlcv
decodeApiOhlcv =
    decode ApiOhlcv
        |> required "at" string
        |> required "open" (maybe float)
        |> required "high" (maybe float)
        |> required "low" (maybe float)
        |> required "close" (maybe float)
        |> required "volume" int
        |> required "source" (maybe string)

encodeApiOhlcv : ApiOhlcv -> Json.Encode.Value
encodeApiOhlcv x =
    Json.Encode.object
        [ ( "at", Json.Encode.string x.at )
        , ( "open", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.float) x.open )
        , ( "high", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.float) x.high )
        , ( "low", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.float) x.low )
        , ( "close", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.float) x.close )
        , ( "volume", Json.Encode.int x.volume )
        , ( "source", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.source )
        ]

type alias NoContent = {}

putApiV1PublishZmqByMarketCode : String -> String -> Http.Request (NoContent)
putApiV1PublishZmqByMarketCode header_Authorization capture_marketCode =
    Http.request
        { method =
            "PUT"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                ]
        , url =
            String.join "/"
                [ "https://tractor.ak1211.com"
                , "api"
                , "v1"
                , "publish"
                , "zmq"
                , capture_marketCode |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectStringResponse
                (\{ body } ->
                    if String.isEmpty body then
                        Ok NoContent
                    else
                        Err "Expected the response body to be empty"
                )
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postApiV1StocksHistoryAll : Http.Request (NoContent)
postApiV1StocksHistoryAll =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ "https://tractor.ak1211.com"
                , "api"
                , "v1"
                , "stocks"
                , "history"
                , "all"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectStringResponse
                (\{ body } ->
                    if String.isEmpty body then
                        Ok NoContent
                    else
                        Err "Expected the response body to be empty"
                )
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getApiV1StocksHistoryByMarketCode : String -> String -> TimeFrame -> Maybe (QueryLimit) -> Http.Request (List (ApiOhlcv))
getApiV1StocksHistoryByMarketCode header_Authorization capture_marketCode query_tf query_limit =
    let
        params =
            List.filter (not << String.isEmpty)
                [ Just query_tf
                    |> Maybe.map (toString >> Http.encodeUri >> (++) "tf=")
                    |> Maybe.withDefault ""
                , query_limit
                    |> Maybe.map (toString >> Http.encodeUri >> (++) "limit=")
                    |> Maybe.withDefault ""
                ]
    in
        Http.request
            { method =
                "GET"
            , headers =
                List.filterMap identity
                    [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                    ]
            , url =
                String.join "/"
                    [ "https://tractor.ak1211.com"
                    , "api"
                    , "v1"
                    , "stocks"
                    , "history"
                    , capture_marketCode |> Http.encodeUri
                    ]
                ++ if List.isEmpty params then
                       ""
                   else
                       "?" ++ String.join "&" params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson (list decodeApiOhlcv)
            , timeout =
                Nothing
            , withCredentials =
                False
            }

putApiV1StocksHistoryByMarketCode : String -> String -> TimeFrame -> List (ApiOhlcv) -> Http.Request (List (ApiOhlcv))
putApiV1StocksHistoryByMarketCode header_Authorization capture_marketCode query_tf body =
    let
        params =
            List.filter (not << String.isEmpty)
                [ Just query_tf
                    |> Maybe.map (toString >> Http.encodeUri >> (++) "tf=")
                    |> Maybe.withDefault ""
                ]
    in
        Http.request
            { method =
                "PUT"
            , headers =
                List.filterMap identity
                    [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                    ]
            , url =
                String.join "/"
                    [ "https://tractor.ak1211.com"
                    , "api"
                    , "v1"
                    , "stocks"
                    , "history"
                    , capture_marketCode |> Http.encodeUri
                    ]
                ++ if List.isEmpty params then
                       ""
                   else
                       "?" ++ String.join "&" params
            , body =
                Http.jsonBody ((Json.Encode.list << List.map encodeApiOhlcv) body)
            , expect =
                Http.expectJson (list decodeApiOhlcv)
            , timeout =
                Nothing
            , withCredentials =
                False
            }

patchApiV1StocksHistoryByMarketCode : String -> String -> TimeFrame -> List (ApiOhlcv) -> Http.Request (List (ApiOhlcv))
patchApiV1StocksHistoryByMarketCode header_Authorization capture_marketCode query_tf body =
    let
        params =
            List.filter (not << String.isEmpty)
                [ Just query_tf
                    |> Maybe.map (toString >> Http.encodeUri >> (++) "tf=")
                    |> Maybe.withDefault ""
                ]
    in
        Http.request
            { method =
                "PATCH"
            , headers =
                List.filterMap identity
                    [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                    ]
            , url =
                String.join "/"
                    [ "https://tractor.ak1211.com"
                    , "api"
                    , "v1"
                    , "stocks"
                    , "history"
                    , capture_marketCode |> Http.encodeUri
                    ]
                ++ if List.isEmpty params then
                       ""
                   else
                       "?" ++ String.join "&" params
            , body =
                Http.jsonBody ((Json.Encode.list << List.map encodeApiOhlcv) body)
            , expect =
                Http.expectJson (list decodeApiOhlcv)
            , timeout =
                Nothing
            , withCredentials =
                False
            }

deleteApiV1StocksHistoryByMarketCode : String -> String -> TimeFrame -> List (ApiOhlcv) -> Http.Request (List (ApiOhlcv))
deleteApiV1StocksHistoryByMarketCode header_Authorization capture_marketCode query_tf body =
    let
        params =
            List.filter (not << String.isEmpty)
                [ Just query_tf
                    |> Maybe.map (toString >> Http.encodeUri >> (++) "tf=")
                    |> Maybe.withDefault ""
                ]
    in
        Http.request
            { method =
                "DELETE"
            , headers =
                List.filterMap identity
                    [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                    ]
            , url =
                String.join "/"
                    [ "https://tractor.ak1211.com"
                    , "api"
                    , "v1"
                    , "stocks"
                    , "history"
                    , capture_marketCode |> Http.encodeUri
                    ]
                ++ if List.isEmpty params then
                       ""
                   else
                       "?" ++ String.join "&" params
            , body =
                Http.jsonBody ((Json.Encode.list << List.map encodeApiOhlcv) body)
            , expect =
                Http.expectJson (list decodeApiOhlcv)
            , timeout =
                Nothing
            , withCredentials =
                False
            }

postApiV1Auth : String -> Http.Request (ApiAccessToken)
postApiV1Auth query_code =
    let
        params =
            List.filter (not << String.isEmpty)
                [ Just query_code
                    |> Maybe.map (Http.encodeUri >> (++) "code=")
                    |> Maybe.withDefault ""
                ]
    in
        Http.request
            { method =
                "POST"
            , headers =
                []
            , url =
                String.join "/"
                    [ "https://tractor.ak1211.com"
                    , "api"
                    , "v1"
                    , "auth"
                    ]
                ++ if List.isEmpty params then
                       ""
                   else
                       "?" ++ String.join "&" params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson decodeApiAccessToken
            , timeout =
                Nothing
            , withCredentials =
                False
            }

getApiV1Version : Http.Request (VerRev)
getApiV1Version =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ "https://tractor.ak1211.com"
                , "api"
                , "v1"
                , "version"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeVerRev
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getApiV1Portfolios : Http.Request (List (ApiPortfolio))
getApiV1Portfolios =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ "https://tractor.ak1211.com"
                , "api"
                , "v1"
                , "portfolios"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeApiPortfolio)
        , timeout =
            Nothing
        , withCredentials =
            False
        }