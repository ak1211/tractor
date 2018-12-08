module Main
  ( main
  ) where

import Prelude

import Api (loadCred)
import AppM (class SessionDSL, runAppM)
import Data.Either (either)
import Data.Either.Nested (Either9)
import Data.Functor.Coproduct.Nested (Coproduct9)
import Data.Maybe (Maybe(..), maybe)
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
import Page.Logout as Logout
import Page.NotFound as NotFound
import Page.Portfolio as Portfolio
import Page.Upload as Upload
import Route (Route(..))
import Route as Route
import Routing.Hash as RH
import Session (Session(..))


-- Multiple types of child component


type ChildQuery
  = Coproduct9
    NotFound.Query
    Dashboard.Query
    Login.Query
    Logout.Query
    Upload.Query
    Portfolio.Query
    Charts.Query
    AccountBalance.Query
    ApiDocument.Query


type ChildSlot
  = Either9
    NotFound.Slot
    Dashboard.Slot
    Login.Slot
    Logout.Slot
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


pathToLogout :: ChildPath Logout.Query ChildQuery Logout.Slot ChildSlot
pathToLogout = ChildPath.cp4


pathToUpload :: ChildPath Upload.Query ChildQuery Upload.Slot ChildSlot
pathToUpload = ChildPath.cp5


pathToPortfolio :: ChildPath Portfolio.Query ChildQuery Portfolio.Slot ChildSlot
pathToPortfolio = ChildPath.cp6


pathToCharts :: ChildPath Charts.Query ChildQuery Charts.Slot ChildSlot
pathToCharts = ChildPath.cp7


pathToAccountBalance :: ChildPath AccountBalance.Query ChildQuery AccountBalance.Slot ChildSlot
pathToAccountBalance = ChildPath.cp8


pathToApiDocument :: ChildPath ApiDocument.Query ChildQuery ApiDocument.Slot ChildSlot
pathToApiDocument = ChildPath.cp9


-- STATE


type State =
  { route :: Route
  }


-- QUERY


data Query a
  = Goto Route a
  | Initialize a
  | Finalize a


-- PARENT COMPONENT


ui :: forall m. MonadAff m => SessionDSL m => H.Component HH.HTML Query Unit Void m
ui = H.lifecycleParentComponent
  { initialState: const init
  , render
  , eval
  , initializer: Just (H.action Initialize)
  , finalizer: Just (H.action Finalize)
  , receiver: const Nothing
  }
  where
  init =
    { route: Root
    }

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
  render st = case st.route of
    Root ->
      HH.slot' pathToNotFound NotFound.Slot NotFound.component unit absurd
      
    Dashboard ->
      HH.slot' pathToDashboard Dashboard.Slot Dashboard.component unit absurd
      
    Login ->
      HH.slot' pathToLogin Login.Slot Login.component unit absurd
      
    Logout->
      HH.slot' pathToLogout Logout.Slot Logout.component unit absurd
      
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
    (Goto newRoute next) -> do
      H.modify_ \st -> st { route = newRoute }
      pure next

    Initialize next -> do
      hash <- H.liftEffect RH.getHash
      let route = either (const Nothing) Just $ RH.match Route.routing hash
      let s = case route of
                Nothing -> "fail"
                Just Root -> "root"
                Just Dashboard -> "dashboard"
                Just Login -> "login"
                Just Logout -> "logout"
                Just Charts -> "charts"
                Just Upload -> "upload"
                Just ApiDocument -> "Apidoc"
                Just AccountBalance -> "acc balance"
                Just Portfolio -> "portfolio"
      H.liftEffect $ log hash
      H.liftEffect $ log s
      case route of
        Nothing -> do
          H.liftEffect $ log "redirect"
          H.liftEffect $ Route.locationReplace Dashboard
        Just _ ->
          H.liftEffect $ log ""
      pure next

    Finalize next -> do
      H.liftEffect $ log "bye"
      pure next


routeSignal :: H.HalogenIO Query Void Aff -> Aff (Effect Unit)
routeSignal driver =
  liftEffect $ RH.matches Route.routing hashChanged
  where
  hashChanged _ newRoute = do
    void <<< launchAff <<< driver.query <<< H.action <<< Goto $ newRoute
    pure unit


-- MAIN


main :: Effect Unit
main = HA.runHalogenAff do
  session <- maybe Guest LoggedIn <$> liftEffect loadCred
  let uiWithSession = H.hoist (flip runAppM session) ui
  body <- HA.awaitBody
  driver <- runUI uiWithSession unit body
  forkAff (routeSignal driver)

