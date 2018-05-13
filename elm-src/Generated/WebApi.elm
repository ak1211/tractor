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

putApiV1PublishZmqByCode : String -> Http.Request (NoContent)
putApiV1PublishZmqByCode capture_code =
    Http.request
        { method =
            "PUT"
        , headers =
            []
        , url =
            String.join "/"
                [ "http://localhost:8739"
                , "api"
                , "v1"
                , "publish"
                , "zmq"
                , capture_code |> Http.encodeUri
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
                [ "http://localhost:8739"
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

getApiV1Portfolios : Http.Request (List (Portfolio))
getApiV1Portfolios =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ "http://localhost:8739"
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

postApiV1QuotesAllUpdate : Http.Request (NoContent)
postApiV1QuotesAllUpdate =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ "http://localhost:8739"
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

getApiV1QuotesByCode : String -> Http.Request (List (Ohlcv))
getApiV1QuotesByCode capture_code =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ "http://localhost:8739"
                , "api"
                , "v1"
                , "quotes"
                , capture_code |> Http.encodeUri
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