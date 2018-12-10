module Route
  ( Route(..)
  , routing
  , href
  , locationReplace
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (intercalate, oneOf)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Effect (Effect)
import Halogen.HTML.Properties as HP
import Routing.Match (Match, lit, param, root)
import Web.HTML as DOM
import Web.HTML.Location as WHL
import Web.HTML.Window as Window
import Api (AuthRedirectParam)


-- ROUTING


data Route
  = AuthRedirect (Either String AuthRedirectParam)
  | Dashboard
  | Login
  | Logout
  | Upload
  | Portfolio
  | Charts
  | AccountBalance
  | ApiDocument


derive instance eqRoute :: Eq Route
derive instance genericRoute :: Generic Route _
instance showRoute :: Show Route where
  show = genericShow


routing :: Match Route
routing = oneOf
  [ AuthRedirect <<< Left <$ root <*> (param "err")
  , AuthRedirect <<< Right <$ root <*> ({ code: _, state: _ } <$> (param "code") <*> (param "state"))
  , Dashboard <$ (root *> lit "#" *> lit "dashboard")
  , Login <$ (root *> lit "#" *> lit "login")
  , Logout <$ (root *> lit "#" *> lit "logout")
  , Upload <$ (root *> lit "#" *> lit "upload")
  , Portfolio <$ (root *> lit "#" *> lit "portfolio")
  , Charts <$ (root *> lit "#" *> lit "charts")
  , AccountBalance <$ (root *> lit "#" *> lit "account-balance")
  , ApiDocument <$ (root *> lit "#" *> lit "api-document")
  ]


-- INTERNAL


routeToString :: Route -> String
routeToString page =
  "#/" <> intercalate "/" pieces
  where
  pieces = case page of
    AuthRedirect _ ->
      []
    Dashboard ->
      [ "dashboard" ]
    Login ->
      [ "login" ]
    Logout ->
      [ "logout" ]
    Upload -> 
      [ "upload" ]
    Portfolio -> 
      [ "portfolio" ]
    Charts -> 
      [ "charts" ]
    AccountBalance -> 
      [ "account-balance" ]
    ApiDocument -> 
      [ "api-document" ]


-- PUBLIC HELPERS


href :: forall r i. Route -> HP.IProp (href :: String | r) i
href targetRoute =
  HP.href (routeToString targetRoute)


locationReplace :: Route -> Effect Unit
locationReplace route =
  DOM.window >>= Window.location >>= WHL.replace (routeToString route)
