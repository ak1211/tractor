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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module Main where
import           Data.Proxy           (Proxy (..))
import qualified Elm
import qualified Servant.Docs
import qualified Servant.Elm
import qualified Shelly

import qualified NetService.ApiTypes  as ApiTypes
import qualified NetService.WebServer as WebServer
import           VerRev               (VerRev)

elmOpts :: Servant.Elm.ElmOptions
elmOpts =
    Servant.Elm.defElmOptions
    { Servant.Elm.urlPrefix = Servant.Elm.Static "https://tractor.ak1211.com"
    }

spec :: Servant.Elm.Spec
spec =
    Servant.Elm.Spec ["Generated", "WebApi"]
        ( Servant.Elm.defElmImports
        --
        : Elm.toElmTypeSource    (Proxy :: Proxy VerRev)
        : Elm.toElmDecoderSource (Proxy :: Proxy VerRev)
        : Elm.toElmEncoderSource (Proxy :: Proxy VerRev)
        --
        : Elm.toElmTypeSource    (Proxy :: Proxy ApiTypes.OAuthReply)
        : Elm.toElmDecoderSource (Proxy :: Proxy ApiTypes.OAuthReply)
        : Elm.toElmEncoderSource (Proxy :: Proxy ApiTypes.OAuthReply)
        --
        : Elm.toElmTypeSource    (Proxy :: Proxy ApiTypes.Portfolio)
        : Elm.toElmDecoderSource (Proxy :: Proxy ApiTypes.Portfolio)
        : Elm.toElmEncoderSource (Proxy :: Proxy ApiTypes.Portfolio)
        --
        : Elm.toElmTypeSource    (Proxy :: Proxy ApiTypes.Ohlcv)
        : Elm.toElmDecoderSource (Proxy :: Proxy ApiTypes.Ohlcv)
        : Elm.toElmEncoderSource (Proxy :: Proxy ApiTypes.Ohlcv)
        --
        : "type alias NoContent = {}"
        : Servant.Elm.generateElmForAPIWith elmOpts  (Proxy :: Proxy WebServer.WebApi))

webApiDocs :: Servant.Docs.API
webApiDocs =
    Servant.Docs.docs WebServer.proxyWebApiServer

webApiDocMarkdown :: String
webApiDocMarkdown =
    Servant.Docs.markdown webApiDocs

main :: IO ()
main = do
    Shelly.shelly $ Shelly.mkdir_p "elm-src/Generated"
    Servant.Elm.specsToDir [spec] "elm-src"
    writeFile "elm-src/public/WebApiDocument.md" webApiDocMarkdown

