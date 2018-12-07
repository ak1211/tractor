module Api
  ( Cred
  , credUsername
  , getWebApiDocument
  , getApiV1AuthClientid
  , getApiV1Version
  , getApiV1Health
  , module NetService.ApiTypes
  ) where

import Affjax as AX
import Affjax.ResponseFormat (ResponseFormatError(..))
import Affjax.ResponseFormat as AXRF
import Control.Monad.Except (runExcept)
import Data.Array as A
import Data.Bifunctor as Bifunctor
import Data.Either (Either(Left, Right))
import Data.HTTP.Method (Method(GET))
import Data.Maybe (Maybe(..))
import Data.String as S
import Effect.Aff (Aff)
import Foreign (MultipleErrors, renderForeignError)
import Foreign.Generic as FG
import Foreign.Generic.Types (Options)
import NetService.ApiTypes (ApiOhlcv(..), ApiPortfolio(..), AuthClientId(..), AuthTempCode(..), AuthenticatedUser(..), RespAuth(..), SystemHealth(..), SystemSignal, VerRev(..), _ApiOhlcv, _ApiPortfolio, _AuthClientId, _AuthTempCode, _AuthenticatedUser, _RespAuth, _SystemHealth, _VerRev)
import Prelude (map, pure, (#), ($), (<<<), (>>=))


-- CRED


data Cred = Cred
  { username :: String
  , jwt :: String
  }


credUsername :: Cred -> String
credUsername (Cred cred) = cred.username


-- API ACCESSOR


renderError :: MultipleErrors -> String
renderError =
  S.joinWith "" <<< A.fromFoldable <<< map renderForeignError


options :: Options
options = FG.defaultOptions { unwrapSingleConstructors = true }


getWebApiDocument :: Aff (AX.Response (Either ResponseFormatError String))
getWebApiDocument =
  AX.request
    { method: Left GET
    , url: "https://tractor.ak1211.com/public/WebApiDocument.md"
    , headers: []
    , content: Nothing
    , username: Nothing
    , password: Nothing
    , withCredentials: false
    , responseFormat: AXRF.string
    }


getApiV1AuthClientid :: Aff (AX.Response (Either String AuthClientId))
getApiV1AuthClientid =
  AX.request
    { method: Left GET
    , url: "https://tractor.ak1211.com/api/v1/auth/clientid"
    , headers: []
    , content: Nothing
    , username: Nothing
    , password: Nothing
    , withCredentials: false
    , responseFormat: AXRF.string
    }
  >>= \res -> pure $ case res.body of
    Left (ResponseFormatError a _) ->
      res { body = Left (renderForeignError a) }
    Right a ->
      res { body = decoder a }
  where
  decoder original =
    FG.genericDecodeJSON options original
    # runExcept
    # Bifunctor.lmap renderError


getApiV1Version :: Aff (AX.Response (Either String VerRev))
getApiV1Version =
  AX.request
    { method: Left GET
    , url: "https://tractor.ak1211.com/api/v1/version"
    , headers: []
    , content: Nothing
    , username: Nothing
    , password: Nothing
    , withCredentials: false
    , responseFormat: AXRF.string
    }
  >>= \res ->
    pure $ case res.body of
      Left (ResponseFormatError a _) ->
        res { body = Left (renderForeignError a) }
      Right a ->
        res { body = decoder a }
  where
  decoder original =
    FG.genericDecodeJSON options original
    # runExcept
    # Bifunctor.lmap renderError


getApiV1Health :: Aff (AX.Response (Either String SystemHealth))
getApiV1Health =
  AX.request
    { method: Left GET
    , url: "https://tractor.ak1211.com/api/v1/health"
    , headers: []
    , content: Nothing
    , username: Nothing
    , password: Nothing
    , withCredentials: false
    , responseFormat: AXRF.string
    }
  >>= \res ->
    pure $ case res.body of
      Left (ResponseFormatError a _) ->
        res { body = Left (renderForeignError a) }
      Right a ->
        res { body = decoder a }
  where
  decoder original =
    FG.genericDecodeJSON options original
    # runExcept
    # Bifunctor.lmap renderError
