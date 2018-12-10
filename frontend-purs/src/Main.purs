module Main
  ( main
  ) where

import Prelude

import Affjax as AX
import Api (AuthClientId(..), AuthRedirectParam, RespAuth(..), credFromJwt, getApiV1AuthClientid, getApiV1Token, loadCred, logout, storeCred, username)
import AppM (class SessionDSL, runAppM)
import Data.Either (Either(..), either)
import Data.Either.Nested (Either8)
import Data.Functor.Coproduct.Nested (Coproduct8)
import Data.Maybe (Maybe(..))
import Data.String.Base64 as Base64
import Effect (Effect)
import Effect.Aff (Aff, launchAff, forkAff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.Component.ChildPath (ChildPath)
import Halogen.Component.ChildPath as ChildPath
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Page.AccountBalance as AccountBalance
import Page.ApiDocument as ApiDocument
import Page.Charts as Charts
import Page.Dashboard as Dashboard
import Page.Login as Login
import Page.NotFound as NotFound
import Page.Portfolio as Portfolio
import Page.Upload as Upload
import Route (Route(..))
import Route as Route
import Routing.PushState as PushState
import Session (Session(..))


-- Multiple types of child component


type ChildQuery
  = Coproduct8
    NotFound.Query
    Dashboard.Query
    Login.Query
    Upload.Query
    Portfolio.Query
    Charts.Query
    AccountBalance.Query
    ApiDocument.Query


type ChildSlot
  = Either8
    NotFound.Slot
    Dashboard.Slot
    Login.Slot
    Upload.Slot
    Portfolio.Slot
    Charts.Slot
    AccountBalance.Slot
    ApiDocument.Slot


pathToNotFound :: ChildPath NotFound.Query ChildQuery NotFound.Slot ChildSlot
pathToNotFound = ChildPath.cp1


pathToDashboard :: ChildPath Dashboard.Query ChildQuery Dashboard.Slot ChildSlot
pathToDashboard = ChildPath.cp2


pathToLogin :: ChildPath Login.Query ChildQuery Login.Slot ChildSlot
pathToLogin = ChildPath.cp3


pathToUpload :: ChildPath Upload.Query ChildQuery Upload.Slot ChildSlot
pathToUpload = ChildPath.cp4


pathToPortfolio :: ChildPath Portfolio.Query ChildQuery Portfolio.Slot ChildSlot
pathToPortfolio = ChildPath.cp5


pathToCharts :: ChildPath Charts.Query ChildQuery Charts.Slot ChildSlot
pathToCharts = ChildPath.cp6


pathToAccountBalance :: ChildPath AccountBalance.Query ChildQuery AccountBalance.Slot ChildSlot
pathToAccountBalance = ChildPath.cp7


pathToApiDocument :: ChildPath ApiDocument.Query ChildQuery ApiDocument.Slot ChildSlot
pathToApiDocument = ChildPath.cp8


-- STATE


type State =
  { route :: Route
  }


-- QUERY


data Query a
  = Goto Route a


-- PARENT COMPONENT


ui :: forall m. MonadAff m => SessionDSL m => H.Component HH.HTML Query Unit Void m
ui = H.parentComponent
  { initialState: const init
  , render
  , eval
  , receiver: const Nothing
  }
  where
  init =
    { route: Dashboard
    }

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
  render st = case st.route of
    AuthRedirect (Left err) ->
      HH.div_ [ HH.text err ]

    AuthRedirect (Right _) ->
      HH.div_ []
      
    Dashboard ->
      HH.slot' pathToDashboard Dashboard.Slot Dashboard.component unit absurd
      
    Login ->
      HH.slot' pathToLogin Login.Slot Login.component unit absurd
      
    Logout->
      HH.div_ []
      
    Upload ->
      HH.slot' pathToUpload Upload.Slot Upload.component unit absurd
      
    Portfolio ->
      HH.slot' pathToPortfolio Portfolio.Slot Portfolio.component unit absurd
      
    Charts ->
      HH.slot' pathToCharts Charts.Slot Charts.component unit absurd
      
    AccountBalance ->
      HH.slot' pathToAccountBalance AccountBalance.Slot AccountBalance.component unit absurd
      
    ApiDocument ->
      HH.slot' pathToApiDocument ApiDocument.Slot ApiDocument.component unit absurd

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
  eval = case _ of
    Goto route next -> do
      newRoute <- case route of
                    AuthRedirect (Right param) -> do
                      newAccessToken <- H.liftAff $ getAccessToken param
                      case newAccessToken.body of
                        Left a ->
                          pure $ AuthRedirect (Left a)
                        Right (RespAuth token) -> do
                          -- store the access token to localstorage
                          H.liftEffect $ log token.accessToken
                          case credFromJwt token.accessToken of
                            Left a ->
                              H.liftEffect $ log "access token JWT parse error"
                            Right cred ->
                              H.liftEffect $ storeCred cred
                          -- redirect
                          H.liftEffect $ Route.redirectTo Dashboard
                          pure Dashboard
                    Logout -> do
                      H.liftEffect logout
                      -- redirect
                      H.liftEffect $ Route.redirectTo Dashboard
                      pure Dashboard
                    _ -> 
                      pure route
      H.modify_ _ { route = newRoute }
      pure next


getAccessToken :: AuthRedirectParam -> Aff (AX.Response (Either String RespAuth))
getAccessToken param = do
  resp <- getApiV1AuthClientid
  either (handleFail resp) go resp.body
  where

  handleFail resp msg =
    pure $ resp { body = Left msg }

  go (AuthClientId x) =
    let user = x.clientid
        pass = param.code
        b64e = Base64.encode (user <> ":" <> pass)
        auth = "Basic " <> b64e
    in do
    getApiV1Token auth


-- MAIN


main :: Effect Unit
main = HA.runHalogenAff do
  maybeEitherCred <- liftEffect loadCred
  session <- case maybeEitherCred of
              Nothing -> 
                pure Guest
              Just a -> do
                liftEffect $ log ("username: " <> username a)
                pure $ LoggedIn a
  let uiWithSession = H.hoist (flip runAppM session) ui
  body <- HA.awaitBody
  driver <- runUI uiWithSession unit body
  forkAff (routeSignal driver)
  where

  routeSignal :: H.HalogenIO Query Void Aff -> Aff (Effect Unit)
  routeSignal driver = do
    interface <- H.liftEffect PushState.makeInterface
    H.liftEffect $ PushState.matches Route.routing pathChanged interface
    where
    pathChanged _ newRoute = do
      void <<< launchAff <<< driver.query <<< H.action <<< Goto $ newRoute
      pure unit
