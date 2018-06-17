module Generated.WebApi exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String


type alias VerRev =
    { version : String
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
        |> required "gitBranch" string
        |> required "gitHash" string
        |> required "gitCommitDate" string
        |> required "gitCommitCount" string
        |> required "gitStatus" string

encodeVerRev : VerRev -> Json.Encode.Value
encodeVerRev x =
    Json.Encode.object
        [ ( "version", Json.Encode.string x.version )
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

type alias Portfolio =
    { code : String
    , caption : Maybe (String)
    , updateAt : Maybe (String)
    }

decodePortfolio : Decoder Portfolio
decodePortfolio =
    decode Portfolio
        |> required "code" string
        |> required "caption" (maybe string)
        |> required "updateAt" (maybe string)

encodePortfolio : Portfolio -> Json.Encode.Value
encodePortfolio x =
    Json.Encode.object
        [ ( "code", Json.Encode.string x.code )
        , ( "caption", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.caption )
        , ( "updateAt", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.updateAt )
        ]

type alias Ohlcv =
    { at : String
    , code : String
    , open : Maybe (Float)
    , high : Maybe (Float)
    , low : Maybe (Float)
    , close : Maybe (Float)
    , volume : Int
    , source : Maybe (String)
    }

decodeOhlcv : Decoder Ohlcv
decodeOhlcv =
    decode Ohlcv
        |> required "at" string
        |> required "code" string
        |> required "open" (maybe float)
        |> required "high" (maybe float)
        |> required "low" (maybe float)
        |> required "close" (maybe float)
        |> required "volume" int
        |> required "source" (maybe string)

encodeOhlcv : Ohlcv -> Json.Encode.Value
encodeOhlcv x =
    Json.Encode.object
        [ ( "at", Json.Encode.string x.at )
        , ( "code", Json.Encode.string x.code )
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

getApiV1Portfolios : String -> Http.Request (List (Portfolio))
getApiV1Portfolios header_Authorization =
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
                , "portfolios"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodePortfolio)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postApiV1QuotesAllUpdate : String -> Http.Request (NoContent)
postApiV1QuotesAllUpdate header_Authorization =
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
                , "quotes"
                , "all"
                , "update"
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

getApiV1QuotesByMarketCode : String -> String -> Http.Request (List (Ohlcv))
getApiV1QuotesByMarketCode header_Authorization capture_marketCode =
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
                , "quotes"
                , capture_marketCode |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeOhlcv)
        , timeout =
            Nothing
        , withCredentials =
            False
        }