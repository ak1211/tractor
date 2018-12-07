module Route
  (Route(..)
  , routing
  , href
  , locationReplace
  ) where

import Prelude

import Data.Foldable (intercalate, oneOf)
import Halogen.HTML.Properties as HP
import Routing.Match (Match, root, lit, end)
import Web.HTML as DOM
import Web.HTML.Location as WHL
import Web.HTML.Window as Window
import Effect (Effect)


-- ROUTING


data Route
  = Root
  | Dashboard
  | Login
  | Logout
  | Upload
  | Portfolio
  | Charts
  | AccountBalance
  | ApiDocument


routing :: Match Route
routing = oneOf
  [ Root <$ (root *> end)
  , Dashboard <$ (root *> lit "dashboard")
  , Login <$ (root *> lit "login")
  , Logout <$ (root *> lit "logout")
  , Upload <$ (root *> lit "upload")
  , Portfolio <$ (root *> lit "portfolio")
  , Charts <$ (root *> lit "charts")
  , AccountBalance <$ (root *> lit "account-balance")
  , ApiDocument <$ (root *> lit "api-document")
  ]


-- INTERNAL


routeToString :: Route -> String
routeToString page =
  "#/" <> intercalate "/" pieces
  where
  pieces = case page of
    Root ->
      []
    Dashboard ->
      [ "dashboard" ]
    Login  ->
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
