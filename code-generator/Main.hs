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
import           Data.Proxy           (Proxy (..))
import qualified Elm
import qualified Servant.Docs
import qualified Servant.Elm
import qualified Shelly

import           NetService.ApiTypes  (ApiAccessToken, ApiOhlcv, ApiPortfolio,
                                       AuthenticatedUser, VerRev)
import           NetService.WebServer (ApiForDocument, ApiForFrontend)


elmOpts :: Servant.Elm.ElmOptions
elmOpts =
    Servant.Elm.defElmOptions
    { Servant.Elm.urlPrefix = Servant.Elm.Static "https://tractor.ak1211.com"
    }

spec :: Servant.Elm.Spec
spec =
    Servant.Elm.Spec ["Generated", "WebApi"]
        ( Servant.Elm.defElmImports
        : "type alias AccessToken = String"
        : "type alias TimeFrame = String"
        : "type alias QueryLimit = Int"
        : "type alias MarketCode = String"
        : "type alias AuthzValue = String"
        : "makeAuthorizationHeader : AccessToken -> AuthzValue"
        : "makeAuthorizationHeader token = \"Bearer \" ++ token"
        --
        : Elm.toElmTypeSource    (Proxy :: Proxy VerRev)
        : Elm.toElmDecoderSource (Proxy :: Proxy VerRev)
        : Elm.toElmEncoderSource (Proxy :: Proxy VerRev)
        --
        : Elm.toElmTypeSource    (Proxy :: Proxy AuthenticatedUser)
        : Elm.toElmDecoderSource (Proxy :: Proxy AuthenticatedUser)
        : Elm.toElmEncoderSource (Proxy :: Proxy AuthenticatedUser)
        --
        : Elm.toElmTypeSource    (Proxy :: Proxy ApiAccessToken)
        : Elm.toElmDecoderSource (Proxy :: Proxy ApiAccessToken)
        : Elm.toElmEncoderSource (Proxy :: Proxy ApiAccessToken)
        --
        : Elm.toElmTypeSource    (Proxy :: Proxy ApiPortfolio)
        : Elm.toElmDecoderSource (Proxy :: Proxy ApiPortfolio)
        : Elm.toElmEncoderSource (Proxy :: Proxy ApiPortfolio)
        --
        : Elm.toElmTypeSource    (Proxy :: Proxy ApiOhlcv)
        : Elm.toElmDecoderSource (Proxy :: Proxy ApiOhlcv)
        : Elm.toElmEncoderSource (Proxy :: Proxy ApiOhlcv)
        --
        : "type alias NoContent = {}"
        : Servant.Elm.generateElmForAPIWith elmOpts (Proxy :: Proxy ApiForFrontend)
        )

webApiDocs :: Servant.Docs.API
webApiDocs =
    Servant.Docs.docs (Proxy :: Proxy ApiForDocument)

webApiDocMarkdown :: String
webApiDocMarkdown =
    Servant.Docs.markdown webApiDocs

main :: IO ()
main = do
    Shelly.shelly $ Shelly.mkdir_p "elm-src/Generated"
    Servant.Elm.specsToDir [spec] "elm-src"
    writeFile "elm-src/public/WebApiDocument.md" webApiDocMarkdown

