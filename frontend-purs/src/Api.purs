module Api
  ( AuthRedirectParam(..)
  , Jwt
  , Cred
  , username
  , EitherCred
  , credFromJwt
  , loadCred
  , storeCred
  , logout
  , getWebApiDocument
  , getApiV1Token
  , getApiV1AuthClientid
  , getApiV1Version
  , getApiV1Health
  , module NetService.ApiTypes
  ) where

import Prelude

import Affjax as AX
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat (ResponseFormatError(..))
import Affjax.ResponseFormat as AXRF
import Control.Monad.Except (runExcept)
import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Core as AC
import Data.Argonaut.Parser as AP
import Data.Array as A
import Data.Bifunctor as Bifunctor
import Data.Either (Either(Right, Left), either)
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(GET))
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (MultipleErrors, renderForeignError)
import Foreign.Generic as FG
import Foreign.Generic.Class (class GenericDecode)
import Foreign.Generic.Types (Options)
import Foreign.Object as StrMap
import Jwt (JwtError)
import Jwt as Jwt
import NetService.ApiTypes (ApiOhlcv(..), ApiPortfolio(..), AuthClientId(..), AuthTempCode(..), AuthenticatedUser(..), RespAuth(..), SystemHealth(..), SystemSignal, VerRev(..), _ApiOhlcv, _ApiPortfolio, _AuthClientId, _AuthTempCode, _AuthenticatedUser, _RespAuth, _SystemHealth, _VerRev)
import Web.HTML as W
import Web.HTML.Window as WW
import Web.Storage.Storage as Storage


type AuthRedirectParam =
  { code :: String
  , state :: String
  }



-- JWT
type Jwt = String


-- CRED
newtype Cred = MkCred
  { username :: String
  , token :: Jwt
  }


username :: Cred -> String
username (MkCred cred) = cred.username


type EitherCred = Either (JwtError String) Cred


credFromJwt :: Jwt -> EitherCred
credFromJwt jwt =
  Jwt.decodeWith decoder jwt
  # Bifunctor.rmap pack
  where
  pack (AuthenticatedUser a) =
    MkCred { username: a.userRealName, token: jwt }

  decoder :: Json -> Either String AuthenticatedUser
  decoder json =
    AC.caseJsonObject Nothing (StrMap.lookup "dat") json
    # case _ of
      Nothing ->
        Left "Json Web Token has 'dat' claims required"
      Just dat ->
        AC.stringify dat
        # FG.genericDecodeJSON options
        # runExcept
        # Bifunctor.lmap renderError


-- PERSISTENCE


loadCred :: Effect (Maybe Cred)
loadCred =
  getItem <#> fromItem >>> fromJson
  where

  getItem :: Effect (Maybe String)
  getItem =
    W.window
    >>= WW.localStorage
    >>= Storage.getItem "store"

  fromItem :: Maybe String -> Maybe Json
  fromItem maybeItem = do
    item <- maybeItem
    either (const Nothing) Just $ AP.jsonParser item
    >>= AC.caseJsonObject Nothing (StrMap.lookup "user")

  fromJson :: Maybe Json -> Maybe Cred
  fromJson maybeJson = do
    credJson <- maybeJson
    unameJson <- AC.caseJsonObject Nothing (StrMap.lookup "username") credJson
    tokenJson <- AC.caseJsonObject Nothing (StrMap.lookup "token") credJson
    cred <- mkCred <$> AC.toString unameJson <*> AC.toString tokenJson
    Just cred

  mkCred :: String -> Jwt -> Cred
  mkCred a b = MkCred { username: a, token: b }


storeCred :: Cred -> Effect Unit
storeCred cred =
  cred # fromCred >>> fromJson >>> stringify >>> setItem
  where

  setItem :: String -> Effect Unit
  setItem item =
    W.window
    >>= WW.localStorage
    >>= Storage.setItem "store" item

  fromCred :: Cred -> Json
  fromCred (MkCred x) =
    AC.fromObject
      ( StrMap.fromFoldable
        [ Tuple "username" (AC.fromString x.username)
        , Tuple "token" (AC.fromString x.token)
        ]
      )

  fromJson :: Json -> Json
  fromJson json =
    AC.jsonSingletonObject "user" json


logout :: Effect Unit
logout =
    W.window
    >>= WW.localStorage
    >>= Storage.removeItem "store"
 

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


genericDecoder :: forall a rep
                  . Generic rep a
                  => GenericDecode a
                  => String
                  -> Either String rep
genericDecoder original =
  FG.genericDecodeJSON options original
  # runExcept
  # Bifunctor.lmap renderError


getApiV1Token :: String -> Aff (AX.Response (Either String RespAuth))
getApiV1Token headerAuthorization =
  AX.request
    { method: Left GET
    , url: "https://tractor.ak1211.com/api/v1/token"
    , headers:
      [ RequestHeader "Authorization" headerAuthorization
      ]
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
      res { body = genericDecoder a }


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
      res { body = genericDecoder a }


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
        res { body = genericDecoder a }


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
        res { body = genericDecoder a }
