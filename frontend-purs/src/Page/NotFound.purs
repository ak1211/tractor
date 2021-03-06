module Page.NotFound
  ( Query(..)
  , Slot(..)
  , component
  ) where

import Prelude

import AppM (class SessionDSL, getSession)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Properties as HP
import Page as Page
import Session (Session(..))


type State =
  { navBarActived :: Boolean
  , session :: Session
  }
  

data Query a
  = ToggleNavBar a
  | Initialize a
 

data Slot = Slot


derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot


component :: forall m. MonadAff m => SessionDSL m => H.Component HH.HTML Query Unit Void m
component =
  H.lifecycleComponent
    { initialState: const initialState
    , render
    , eval
    , initializer: Just (H.action Initialize)
    , finalizer: Nothing
    , receiver: const Nothing
    }
  where

  initialState =
    { navBarActived: false
    , session: Guest
    }

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    ToggleNavBar next -> do
      H.modify_ \st ->
        st { navBarActived = not st.navBarActived }
      pure next

    Initialize next -> do
      newSession <- getSession
      H.modify_ _ { session = newSession }
      --
      pure next


-- RENDER


render :: State -> H.ComponentHTML Query
render state =
  HH.div_
    [ Page.fixedTopNavBar ToggleNavBar state.navBarActived state.session
    , Page.hero "Not Found"
    , HH.section
        [ HP.class_ (HC.ClassName "section")
        ]
        [ HH.div
          [ HP.class_ (HC.ClassName "container")
          ]
          contents
        ]
    , Page.footer
    ]
  where

  contents =
    [ HH.text "NOT FOUND"
    ]
