module Msg exposing (..)

import Http
import Material
import FileReader
import Navigation
import Model
import Generated.WebApi as Api


type Msg
    = Increase
    | Reset
    | DropZoneEntered
    | DropZoneLeaved
    | FilesDropped (List FileReader.File)
    | UrlChange Navigation.Location
    | NewUrl String
      --    | PageTransition Route
    | NewServerVersion (Result Http.Error Api.VerRev)
    | NewPortfolios (Result Http.Error Model.Portfolios)
    | NewHistories (Result Http.Error Model.Histories)
    | NewWebApiDocumentMd (Result Http.Error String)
    | RequestNewHistories String
    | ScrollToTop
    | Nop
    | Mdl (Material.Msg Msg)
