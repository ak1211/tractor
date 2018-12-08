module Api
  ( Jwt
  , Cred
  , username
  , credFromJwt
  , loadCred
  , getWebApiDocument
  , getApiV1AuthClientid
  , getApiV1Version
  , getApiV1Health
  , module NetService.ApiTypes
  ) where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat (ResponseFormatError(..))
import Affjax.ResponseFormat as AXRF
import Control.Monad.Except (runExcept)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as AC
import Data.Array as A
import Data.Bifunctor as Bifunctor
import Data.Either (Either(Left, Right))
import Data.HTTP.Method (Method(GET))
import Data.Maybe (Maybe(..))
import Data.String as S
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Foreign (MultipleErrors, renderForeignError)
import Foreign.Generic as FG
import Foreign.Generic.Types (Options)
import Foreign.Object as StrMap
import Jwt (JwtError(..))
import Jwt as Jwt
import NetService.ApiTypes (ApiOhlcv(..), ApiPortfolio(..), AuthClientId(..), AuthTempCode(..), AuthenticatedUser(..), RespAuth(..), SystemHealth(..), SystemSignal, VerRev(..), _ApiOhlcv, _ApiPortfolio, _AuthClientId, _AuthTempCode, _AuthenticatedUser, _RespAuth, _SystemHealth, _VerRev)
import Web.HTML as W
import Web.HTML.Window as WW
import Web.Storage.Storage as Storage


-- JWT
type Jwt = String


-- CRED
newtype Cred = MkCred
  { username :: String
  , jwt :: Jwt
  }


username :: Cred -> String
username (MkCred cred) = cred.username


credFromJwt :: Jwt -> Either (JwtError String) Cred
credFromJwt jwt =
  Jwt.decodeWith decoder jwt
  # Bifunctor.rmap pack
  where
  pack (AuthenticatedUser a) =
    MkCred { username: a.userRealName, jwt: jwt }

  decoder :: Json -> Either String AuthenticatedUser
  decoder json =
    AC.caseJsonObject Nothing (StrMap.lookup "dat") json
    # case _ of
      Nothing ->
        Left "'dat' claims required"
      Just dat ->
        AC.stringify dat
        # FG.genericDecodeJSON options
        # runExcept
        # Bifunctor.lmap renderError


loadCred :: Effect (Maybe Cred)
loadCred = do
  window <- W.window
  storage <- WW.localStorage window
  maybeItem <- Storage.getItem "store" storage
  logShow maybeItem
  maybeCred <- pure $ map credFromJwt maybeItem
  case maybeCred of
    Nothing -> pure Nothing
    Just (Left (JsonDecodeError err)) -> do
      log "JWT ERROR"
      log err
      pure Nothing
    Just (Left _) -> do
      log "JWT ERROR"
      pure Nothing
    Just (Right a) ->
      pure $ Just a



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
  decoder :: String -> Either String AuthClientId
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
  decoder :: String -> Either String VerRev
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
  decoder :: String -> Either String SystemHealth
  decoder original =
    FG.genericDecodeJSON options original
    # runExcept
    # Bifunctor.lmap renderError
