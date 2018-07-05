module Generated.WebApi exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String


type alias AccessToken = String

type alias TimeFrame = String

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

type alias OAuthReply =
    { accessToken : String
    , scope : String
    , userName : String
    }

decodeOAuthReply : Decoder OAuthReply
decodeOAuthReply =
    decode OAuthReply
        |> required "accessToken" string
        |> required "scope" string
        |> required "userName" string

encodeOAuthReply : OAuthReply -> Json.Encode.Value
encodeOAuthReply x =
    Json.Encode.object
        [ ( "accessToken", Json.Encode.string x.accessToken )
        , ( "scope", Json.Encode.string x.scope )
        , ( "userName", Json.Encode.string x.userName )
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

getApiV1ExchangeTemporaryCodeByTempCode : String -> Http.Request (OAuthReply)
getApiV1ExchangeTemporaryCodeByTempCode capture_tempCode =
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
                , "exchange"
                , "temporary"
                , "code"
                , capture_tempCode |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeOAuthReply
        , timeout =
            Nothing
        , withCredentials =
            False
        }

putApiV1PublishZmqByMarketCode : String -> String -> Http.Request (NoContent)
putApiV1PublishZmqByMarketCode header_Authorization capture_marketCode =
    Http.request
        { method =
            "PUT"
        , headers =
            [ Http.header "Authorization" header_Authorization
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

postApiV1StocksHistoryAll : String -> Http.Request (NoContent)
postApiV1StocksHistoryAll header_Authorization =
    Http.request
        { method =
            "POST"
        , headers =
            [ Http.header "Authorization" header_Authorization
            ]
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

getApiV1StocksHistoryByMarketCode : String -> String -> Maybe (TimeFrame) -> Http.Request (List (ApiOhlcv))
getApiV1StocksHistoryByMarketCode header_Authorization capture_marketCode query_tf =
    let
        params =
            List.filter (not << String.isEmpty)
                [ query_tf
                    |> Maybe.map (toString >> Http.encodeUri >> (++) "tf=")
                    |> Maybe.withDefault ""
                ]
    in
        Http.request
            { method =
                "GET"
            , headers =
                [ Http.header "Authorization" header_Authorization
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

putApiV1StocksHistoryByMarketCode : String -> String -> Maybe (TimeFrame) -> List (ApiOhlcv) -> Http.Request (List (ApiOhlcv))
putApiV1StocksHistoryByMarketCode header_Authorization capture_marketCode query_tf body =
    let
        params =
            List.filter (not << String.isEmpty)
                [ query_tf
                    |> Maybe.map (toString >> Http.encodeUri >> (++) "tf=")
                    |> Maybe.withDefault ""
                ]
    in
        Http.request
            { method =
                "PUT"
            , headers =
                [ Http.header "Authorization" header_Authorization
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

patchApiV1StocksHistoryByMarketCode : String -> String -> Maybe (TimeFrame) -> List (ApiOhlcv) -> Http.Request (List (ApiOhlcv))
patchApiV1StocksHistoryByMarketCode header_Authorization capture_marketCode query_tf body =
    let
        params =
            List.filter (not << String.isEmpty)
                [ query_tf
                    |> Maybe.map (toString >> Http.encodeUri >> (++) "tf=")
                    |> Maybe.withDefault ""
                ]
    in
        Http.request
            { method =
                "PATCH"
            , headers =
                [ Http.header "Authorization" header_Authorization
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

deleteApiV1StocksHistoryByMarketCode : String -> String -> Maybe (TimeFrame) -> List (ApiOhlcv) -> Http.Request (List (ApiOhlcv))
deleteApiV1StocksHistoryByMarketCode header_Authorization capture_marketCode query_tf body =
    let
        params =
            List.filter (not << String.isEmpty)
                [ query_tf
                    |> Maybe.map (toString >> Http.encodeUri >> (++) "tf=")
                    |> Maybe.withDefault ""
                ]
    in
        Http.request
            { method =
                "DELETE"
            , headers =
                [ Http.header "Authorization" header_Authorization
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