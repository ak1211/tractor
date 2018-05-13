module Model exposing (..)

import Material
import Route exposing (Route)
import Generated.WebApi as WebApi
import FileReader


type alias Portfolios =
    List WebApi.Portfolio


type alias Histories =
    List WebApi.Ohlcv


type alias Model =
    { count : Int
    , pageHistory : List Route

    --    , page : Route
    , inDropZone : Bool
    , droppedFiles : List FileReader.File
    , serverVersion : Maybe WebApi.VerRev
    , portfolios : Maybe Portfolios
    , histories : Maybe Histories
    , webApiDocumentMd : String
    , mdl :
        Material.Model

    -- Boilerplate: model store for any and all Mdl components you use.
    }
