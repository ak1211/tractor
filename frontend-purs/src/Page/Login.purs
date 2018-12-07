module Page.Login
  ( Query(..)
  , Slot(..)
  , component
  ) where

import Prelude

import Api (AuthClientId(..), getApiV1AuthClientid)
import AppM (class SessionDSL, getSession)
import Bulma.Columns.Columns as Columns
import Bulma.Common as BC
import Bulma.Elements.Button as Button
import Bulma.Layout.Layout as Layout
import Bulma.Modifiers.Typography as Typo
import CSS.VerticalAlign (VerticalAlign(..), verticalAlign)
import Data.Either (either)
import Data.Maybe (Maybe(..), maybe)
import Data.String as S
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Core as HC
import Halogen.HTML.Properties as HP
import Page as Page
import Session (Session(..))


type State =
  { navBarActived :: Boolean
  , session :: Session
  , authClientId :: Maybe AuthClientId
  }


data Query a
  = ToggleNavBar a
  | Initialize a
  | Finalize a
 

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
    , finalizer: Just (H.action Finalize)
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState =
    { navBarActived: false
    , session: Guest
    , authClientId: Nothing
    }

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    ToggleNavBar next -> do
      H.modify_ \st ->
        st { navBarActived = not st.navBarActived }
      pure next

    Initialize next -> do
      cid <- H.liftAff $ getApiV1AuthClientid
      session <- getSession
      H.modify_
        _ { session = session
          , authClientId = either (const Nothing) Just cid.body
        }
      pure next

    Finalize next -> do
      pure next


-- RENDER


render :: State -> H.ComponentHTML Query
render state =
  HH.div_
    [ Page.fixedTopNavBar ToggleNavBar state.navBarActived state.session
    , Page.hero "Log in"
    , HH.section
        [ HP.class_ <<< HC.ClassName $ BC.runClassName Layout.section
        ]
        [ HH.div
          [ HP.class_ <<< HC.ClassName $ BC.runClassName Layout.container
          ]
          [ maybe (HH.text "Can't get the client ID.") signInWithSlackButton state.authClientId
          ]
        ]
    , Page.footer
    ]


signInWithSlackButton :: forall p i. AuthClientId -> H.HTML p i
signInWithSlackButton (AuthClientId cid) =
  HH.div
    [ HP.class_ <<< HC.ClassName $ BC.runClassNames
        [ Columns.column
        , Typo.hasAlignment Typo.Centered
        , Typo.isSize Typo.Size5
        ]
    ]
    [ HH.a
      [ HP.class_ <<< HC.ClassName $ BC.runClassNames
        [ Button.button
        , Button.isColor Button.Dark
        ]
      , hrefSignInWithSlack "identity.basic" cid.clientid "abcdefg"
      ]
      [ slackMark
      , HH.span [] []
      , HH.text "アカウントでログイン"
      ]
    ]
  where

  hrefSignInWithSlack scope clientid state =
    "https://slack.com/oauth/authorize" <> 
    "?" <>
    S.joinWith "&"
      [ "scope=" <> scope
      , "client_id=" <> clientid
      , "state=" <> state
      ]
    # HP.href


  slackMark =
    HH.img
      [ CSS.style do
          verticalAlign Middle
      , HP.src "https://tractor.ak1211.com/public/assets/Slack_Monochrome_White.svg"
      , HP.alt "Slack"
      , HP.width 80
      ]
