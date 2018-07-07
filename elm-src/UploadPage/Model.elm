{-
   This file is part of Tractor.

   Tractor is free software: you can redistribute it and/or modify
   it under the terms of the GNU Affero General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   Tractor is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Affero General Public License for more details.

   You should have received a copy of the GNU Affero General Public License
   along with Tractor.  If not, see <http://www.gnu.org/licenses/>.
-}
{- |
   Module      :  UploadPage.Model
   Description :  This file is web front main module of Application "Tractor"
   Copyright   :  (c) 2016 Akihiro Yamamoto
   License     :  AGPLv3

   Maintainer  :  https://github.com/ak1211
   Stability   :  unstable
   Portability :  POSIX

   Uploadページのモデル定義です。
-}


module UploadPage.Model
    exposing
        ( FileContent
        , UploadThunk
        , UploadProgress
        , Model
        , initialUploadProgress
        , clearModel
        , init
        )

import Generated.WebApi as WebApi
import Material
import UploadPage.Msg as UploadPage


type alias FileContent =
    { name : String
    , marketCode : WebApi.MarketCode
    , timeFrame : WebApi.TimeFrame
    , ohlcvs : List WebApi.ApiOhlcv
    }


type alias UploadThunk =
    { function : WebApi.MarketCode -> WebApi.TimeFrame -> List WebApi.ApiOhlcv -> Cmd UploadPage.Msg
    , marketCode : WebApi.MarketCode
    , timeFrame : WebApi.TimeFrame
    , ohlcvs : List WebApi.ApiOhlcv
    }


type alias UploadProgress =
    { counter : Int
    , total : Int
    , todo : List UploadThunk
    , done : List UploadThunk
    }


type alias Model =
    { accessToken : Maybe WebApi.AccessToken
    , inDropZone : Bool
    , fileContents : List (Result String FileContent)
    , isStepByStepMode : Bool
    , isPendingUpload : Bool
    , progress : UploadProgress
    , mdl : Material.Model
    }


clearModel : Model -> Model
clearModel model =
    { model
        | inDropZone = False
        , fileContents = []
        , isStepByStepMode = False
        , isPendingUpload = False
        , progress = initialUploadProgress
    }


initialUploadProgress : UploadProgress
initialUploadProgress =
    { counter = 0
    , total = 1
    , todo = []
    , done = []
    }


initialModel : Model
initialModel =
    { accessToken = Nothing
    , inDropZone = False
    , fileContents = []
    , isStepByStepMode = False
    , isPendingUpload = False
    , progress = initialUploadProgress
    , mdl = Material.model
    }


init : ( Model, Cmd UploadPage.Msg )
init =
    initialModel ! [ Material.init UploadPage.Mdl ]
