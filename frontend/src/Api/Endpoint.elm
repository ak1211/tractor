module Api.Endpoint exposing (ApiOhlcv, ApiPortfolio, AuthClientId, AuthTempCode, AuthenticatedUser, AuthzValue, JWT, MarketCode, NoContent, QueryLimit, RespAuth, SystemHealth, SystemSignal, TimeFrame, VerRev, decodeApiOhlcv, decodeApiPortfolio, decodeAuthClientId, decodeAuthTempCode, decodeAuthenticatedUser, decodeRespAuth, decodeSystemHealth, decodeSystemSignal, decodeVerRev, deleteApiV1StocksHistoryByMarketCode, encodeApiOhlcv, encodeApiPortfolio, encodeAuthClientId, encodeAuthTempCode, encodeAuthenticatedUser, encodeRespAuth, encodeVerRev, getApiV1AuthClientid, getApiV1Health, getApiV1Portfolios, getApiV1StocksHistoryByMarketCode, getApiV1Version, makeAuthorizationHeader, patchApiV1StocksHistoryByMarketCode, postApiV1Auth, postApiV1StocksHistoryAll, putApiV1PublishZmqByMarketCode, putApiV1StocksHistoryByMarketCode)

import Http
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import String
import Url


type alias SystemSignal =
    String


decodeSystemSignal : Decoder SystemSignal
decodeSystemSignal =
    string


type alias JWT =
    String


type alias TimeFrame =
    String


type alias QueryLimit =
    Int


type alias MarketCode =
    String


type alias AuthzValue =
    String


makeAuthorizationHeader : JWT -> AuthzValue
makeAuthorizationHeader token =
    "Bearer " ++ token


type alias SystemHealth =
    { system : SystemSignal
    }


decodeSystemHealth : Decoder SystemHealth
decodeSystemHealth =
    succeed SystemHealth
        |> required "system" decodeSystemSignal


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
    succeed VerRev
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
    succeed AuthenticatedUser
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


type alias RespAuth =
    { accessToken : String
    , expiresIn : Int
    , tokenType : String
    }


decodeRespAuth : Decoder RespAuth
decodeRespAuth =
    succeed RespAuth
        |> required "accessToken" string
        |> required "expiresIn" int
        |> required "tokenType" string


encodeRespAuth : RespAuth -> Json.Encode.Value
encodeRespAuth x =
    Json.Encode.object
        [ ( "accessToken", Json.Encode.string x.accessToken )
        , ( "expiresIn", Json.Encode.int x.expiresIn )
        , ( "tokenType", Json.Encode.string x.tokenType )
        ]


type alias ApiPortfolio =
    { code : String
    , caption : Maybe String
    , updateAt : Maybe String
    }


decodeApiPortfolio : Decoder ApiPortfolio
decodeApiPortfolio =
    succeed ApiPortfolio
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
    , open : Maybe Float
    , high : Maybe Float
    , low : Maybe Float
    , close : Maybe Float
    , volume : Int
    , source : Maybe String
    }


decodeApiOhlcv : Decoder ApiOhlcv
decodeApiOhlcv =
    succeed ApiOhlcv
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


type alias AuthTempCode =
    { code : String
    }


decodeAuthTempCode : Decoder AuthTempCode
decodeAuthTempCode =
    succeed AuthTempCode
        |> required "code" string


encodeAuthTempCode : AuthTempCode -> Json.Encode.Value
encodeAuthTempCode x =
    Json.Encode.object
        [ ( "code", Json.Encode.string x.code )
        ]


type alias AuthClientId =
    { clientid : String
    }


decodeAuthClientId : Decoder AuthClientId
decodeAuthClientId =
    succeed AuthClientId
        |> required "clientid" string


encodeAuthClientId : AuthClientId -> Json.Encode.Value
encodeAuthClientId x =
    Json.Encode.object
        [ ( "clientid", Json.Encode.string x.clientid )
        ]


type alias NoContent =
    {}


putApiV1PublishZmqByMarketCode : String -> String -> Http.Request NoContent
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
                , capture_marketCode |> Url.percentEncode
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


postApiV1StocksHistoryAll : Http.Request NoContent
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


getApiV1StocksHistoryByMarketCode : String -> String -> TimeFrame -> Maybe QueryLimit -> Http.Request (List ApiOhlcv)
getApiV1StocksHistoryByMarketCode header_Authorization capture_marketCode query_tf query_limit =
    let
        params =
            List.filter (not << String.isEmpty)
                [ Just query_tf
                    |> Maybe.map (Url.percentEncode >> (++) "tf=")
                    |> Maybe.withDefault ""
                , query_limit
                    |> Maybe.map (String.fromInt >> Url.percentEncode >> (++) "limit=")
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
                , capture_marketCode |> Url.percentEncode
                ]
                ++ (if List.isEmpty params then
                        ""

                    else
                        "?" ++ String.join "&" params
                   )
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeApiOhlcv)
        , timeout =
            Nothing
        , withCredentials =
            False
        }


putApiV1StocksHistoryByMarketCode : String -> String -> TimeFrame -> List ApiOhlcv -> Http.Request (List ApiOhlcv)
putApiV1StocksHistoryByMarketCode header_Authorization capture_marketCode query_tf body =
    let
        params =
            List.filter (not << String.isEmpty)
                [ Just query_tf
                    |> Maybe.map (Url.percentEncode >> (++) "tf=")
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
                , capture_marketCode |> Url.percentEncode
                ]
                ++ (if List.isEmpty params then
                        ""

                    else
                        "?" ++ String.join "&" params
                   )
        , body =
            Http.jsonBody (Json.Encode.list encodeApiOhlcv body)
        , expect =
            Http.expectJson (list decodeApiOhlcv)
        , timeout =
            Nothing
        , withCredentials =
            False
        }


patchApiV1StocksHistoryByMarketCode : String -> String -> TimeFrame -> List ApiOhlcv -> Http.Request (List ApiOhlcv)
patchApiV1StocksHistoryByMarketCode header_Authorization capture_marketCode query_tf body =
    let
        params =
            List.filter (not << String.isEmpty)
                [ Just query_tf
                    |> Maybe.map (Url.percentEncode >> (++) "tf=")
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
                , capture_marketCode |> Url.percentEncode
                ]
                ++ (if List.isEmpty params then
                        ""

                    else
                        "?" ++ String.join "&" params
                   )
        , body =
            Http.jsonBody (Json.Encode.list encodeApiOhlcv body)
        , expect =
            Http.expectJson (list decodeApiOhlcv)
        , timeout =
            Nothing
        , withCredentials =
            False
        }


deleteApiV1StocksHistoryByMarketCode : String -> String -> TimeFrame -> List ApiOhlcv -> Http.Request (List ApiOhlcv)
deleteApiV1StocksHistoryByMarketCode header_Authorization capture_marketCode query_tf body =
    let
        params =
            List.filter (not << String.isEmpty)
                [ Just query_tf
                    |> Maybe.map (Url.percentEncode >> (++) "tf=")
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
                , capture_marketCode |> Url.percentEncode
                ]
                ++ (if List.isEmpty params then
                        ""

                    else
                        "?" ++ String.join "&" params
                   )
        , body =
            Http.jsonBody (Json.Encode.list encodeApiOhlcv body)
        , expect =
            Http.expectJson (list decodeApiOhlcv)
        , timeout =
            Nothing
        , withCredentials =
            False
        }


postApiV1Auth : AuthTempCode -> Http.Request RespAuth
postApiV1Auth body =
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
        , body =
            Http.jsonBody (encodeAuthTempCode body)
        , expect =
            Http.expectJson decodeRespAuth
        , timeout =
            Nothing
        , withCredentials =
            False
        }


getApiV1AuthClientid : Http.Request AuthClientId
getApiV1AuthClientid =
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
                , "auth"
                , "clientid"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeAuthClientId
        , timeout =
            Nothing
        , withCredentials =
            False
        }


getApiV1Version : Http.Request VerRev
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


getApiV1Health : Http.Request SystemHealth
getApiV1Health =
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
                , "health"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeSystemHealth
        , timeout =
            Nothing
        , withCredentials =
            False
        }


getApiV1Portfolios : Http.Request (List ApiPortfolio)
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
