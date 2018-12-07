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
Module      :  Main
Description :
Copyright   :  (c) 2016 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  POSIX

-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData         #-}
module Main where
import           Data.Proxy                               ( Proxy(..) )
import qualified Elm
import qualified Language.PureScript.Bridge    as Purs
import qualified Servant.Docs
import qualified Servant.Elm
import qualified Servant.JS
import qualified Shelly

import           NetService.ApiTypes                      ( ApiOhlcv
                                                          , ApiPortfolio
                                                          , AuthClientId
                                                          , AuthTempCode
                                                          , AuthenticatedUser
                                                          , RespAuth
                                                          , SystemHealth
                                                          , VerRev
                                                          )
import           NetService.WebServer                     ( ApiForDocument
                                                          , ApiForFrontend
                                                          )


elmOpts :: Servant.Elm.ElmOptions
elmOpts = Servant.Elm.defElmOptions
    { Servant.Elm.urlPrefix = Servant.Elm.Static "https://tractor.ak1211.com"
    }

spec :: Servant.Elm.Spec
spec = Servant.Elm.Spec
    ["Api", "Endpoint"]
    ( Servant.Elm.defElmImports
    : "type alias SystemSignal = String"
    : "decodeSystemSignal : Decoder SystemSignal"
    : "decodeSystemSignal = string"
    : "type alias JWT = String"
    : "type alias TimeFrame = String"
    : "type alias RecordsLimit = Int"
    : "type alias MarketCode = String"
    : "type alias AuthzValue = String"
    : "makeAuthorizationHeader : JWT -> AuthzValue"
    : "makeAuthorizationHeader token = \"Bearer \" ++ token"
    --
    : Elm.toElmTypeSource (Proxy :: Proxy SystemHealth)
    : Elm.toElmDecoderSource (Proxy :: Proxy SystemHealth)
    --
    : Elm.toElmTypeSource (Proxy :: Proxy VerRev)
    : Elm.toElmDecoderSource (Proxy :: Proxy VerRev)
    : Elm.toElmEncoderSource (Proxy :: Proxy VerRev)
    --
    : Elm.toElmTypeSource (Proxy :: Proxy AuthenticatedUser)
    : Elm.toElmDecoderSource (Proxy :: Proxy AuthenticatedUser)
    : Elm.toElmEncoderSource (Proxy :: Proxy AuthenticatedUser)
    --
    : Elm.toElmTypeSource (Proxy :: Proxy RespAuth)
    : Elm.toElmDecoderSource (Proxy :: Proxy RespAuth)
    : Elm.toElmEncoderSource (Proxy :: Proxy RespAuth)
    --
    : Elm.toElmTypeSource (Proxy :: Proxy ApiPortfolio)
    : Elm.toElmDecoderSource (Proxy :: Proxy ApiPortfolio)
    : Elm.toElmEncoderSource (Proxy :: Proxy ApiPortfolio)
    --
    : Elm.toElmTypeSource (Proxy :: Proxy ApiOhlcv)
    : Elm.toElmDecoderSource (Proxy :: Proxy ApiOhlcv)
    : Elm.toElmEncoderSource (Proxy :: Proxy ApiOhlcv)
    --
    : Elm.toElmTypeSource (Proxy :: Proxy AuthTempCode)
    : Elm.toElmDecoderSource (Proxy :: Proxy AuthTempCode)
    : Elm.toElmEncoderSource (Proxy :: Proxy AuthTempCode)
    --
    : Elm.toElmTypeSource (Proxy :: Proxy AuthClientId)
    : Elm.toElmDecoderSource (Proxy :: Proxy AuthClientId)
    : Elm.toElmEncoderSource (Proxy :: Proxy AuthClientId)
    --
    : "type alias NoContent = {}"
    : Servant.Elm.generateElmForAPIWith elmOpts
                                        (Proxy :: Proxy ApiForFrontend)
    )

webApiDocs :: Servant.Docs.API
webApiDocs = Servant.Docs.docs (Proxy :: Proxy ApiForDocument)

webApiDocMarkdown :: String
webApiDocMarkdown = Servant.Docs.markdown webApiDocs

main :: IO ()
main = do
    -- generate elm
    Shelly.shelly $ Shelly.mkdir_p "frontend-elm/src/Api"
    Servant.Elm.specsToDir [spec] "frontend-elm/src"
    -- generate javascrpt
    Servant.JS.writeJSForAPI (Proxy :: Proxy ApiForFrontend)
                             axios
                             "frontend-elm/src/Api/Endpoint.js"
    -- generate purescript
    Shelly.shelly $ Shelly.mkdir_p "frontend-purs/src/Api"
    Purs.writePSTypes "frontend-purs/src/Api"
                      (Purs.buildBridge Purs.defaultBridge)
                      pursTypes
    -- generate document
    writeFile "frontend-elm/public/WebApiDocument.md" webApiDocMarkdown
  where
    axios = Servant.JS.axios Servant.JS.defAxiosOptions
    pursTypes =
        [ Purs.mkSumType (Proxy :: Proxy SystemHealth)
        , Purs.mkSumType (Proxy :: Proxy VerRev)
        , Purs.mkSumType (Proxy :: Proxy AuthenticatedUser)
        , Purs.mkSumType (Proxy :: Proxy RespAuth)
        , Purs.mkSumType (Proxy :: Proxy ApiPortfolio)
        , Purs.mkSumType (Proxy :: Proxy ApiOhlcv)
        , Purs.mkSumType (Proxy :: Proxy AuthTempCode)
        , Purs.mkSumType (Proxy :: Proxy AuthClientId)
        ]

