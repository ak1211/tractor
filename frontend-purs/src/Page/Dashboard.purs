module Page.Dashboard
  ( Query(..)
  , Slot(..)
  , component
  ) where

import Prelude

import Api as Api
import AppM (class SessionDSL, getSession)
import Bulma.Common as BC
import Bulma.Elements.Elements as Elements
import Bulma.Elements.Tag as Tag
import Bulma.Form.General as Form
import Bulma.Modifiers.Typography as Typo
import CSS (marginLeft, marginRight, rem)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Core as HC
import Halogen.HTML.Properties as HP
import Page as Page
import Session (Session(..))


data Result3 a = Indefinite | Err String | Ok a


type State =
  { navBarActived :: Boolean
  , systemHealth  :: Result3 Api.SystemHealth
  , systemVersion :: Result3 Api.VerRev
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
    , systemHealth: Indefinite
    , systemVersion: Indefinite
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
      newHealth <- H.liftAff $ Api.getApiV1Health
      H.modify_ _ { systemHealth = either Err Ok newHealth.body }
      --
      newVersion <- H.liftAff $ Api.getApiV1Version
      H.modify_ _ { systemVersion = either Err Ok newVersion.body }
      --
      pure next


-- RENDER


render :: State -> H.ComponentHTML Query
render state =
  HH.div_
    [ Page.fixedTopNavBar ToggleNavBar state.navBarActived state.session
    , Page.hero "Dashboard"
    , HH.section
        [ HP.class_ (HC.ClassName "section")
        ]
        [ HH.div
          [ HP.class_ (HC.ClassName "container")
          ]
          [ statusLine
          , greetings state
          ]
        ]
    , Page.footer
    ]
  where

  statusLine =
    HH.div
      [ HP.classes
        [ HC.ClassName "field"
        , HC.ClassName $ BC.runClassNames
          [ BC.isGrouped
          , Tag.isGroupedMultiline
          ]
        ]
      ]
      case state.systemVersion of
        Ok x ->
          [ healthBadge state.systemHealth
          , control [ versionBadge x ]
          , control [ archBadge x ]
          , control [ osBadge x ]
          , control [ gitReposBadge x ]
          , control [ gitCommitsBadge x ]
          ]
        _ ->
          [ healthBadge state.systemHealth
          ]


-- HELPERS


type LeftRight a =
  { left :: a
  , right :: a
  }


packLR :: forall a. a -> a -> LeftRight a
packLR = { left: _, right: _ }


-- GREETINGS


greetings :: forall p i. State -> H.HTML p i
greetings state =
  HH.div
    [ HP.class_ <<< HC.ClassName $ BC.runClassNames
      [ Elements.notification
      , Typo.hasAlignment Typo.Centered
      , Tag.isColor Tag.Dark
      ]
    ]
    [ HH.p
      [ HP.class_ <<< HC.ClassName $ BC.runClassName (Typo.isSize Typo.Size5)
      ]
      [ HH.text "Maido!"
      , HH.span
        [ CSS.style do
          marginLeft $ rem 1.8
          marginRight $ rem 1.8
        ]
        [ HH.text username
        ]
      , HH.text "san."
      ]
    ]
  where
  username = case state.session of
    LoggedIn a ->
      Api.username a
    Guest ->
      "Guest"


-- FORM CONTROL


control :: forall p i. Array (H.HTML p i) -> H.HTML p i
control =
  HH.p
    [ HP.class_ <<< HC.ClassName $ BC.runClassName Form.control
    ]


-- BADGE


badge :: forall p i. LeftRight Tag.Color -> LeftRight String -> H.HTML p i 
badge col txt =
  HH.div
    [ HP.class_ <<< HC.ClassName $ BC.runClassNames
      [ Tag.tags
      , BC.hasAddons
      ]
    ]
    [ HH.span
      [ HP.class_ <<< HC.ClassName $ BC.runClassNames
        [ Tag.tag
        , Tag.isColor col.left
        ]
      ]
      [ HH.text txt.left
      ]
    , HH.span
      [ HP.class_ <<< HC.ClassName $ BC.runClassNames
        [ Tag.tag
        , Tag.isColor col.right
        ]
      ]
      [ HH.text txt.right
      ]
    ]


infoBadge :: forall p i. String -> String -> H.HTML p i 
infoBadge l r =
  badge (packLR Tag.Dark $ Tag.CommonColor BC.Info) $ packLR l r


versionBadge :: forall p i. Api.VerRev -> H.HTML p i
versionBadge (Api.VerRev v) =
  infoBadge "Version" v.version


archBadge :: forall p i. Api.VerRev -> H.HTML p i
archBadge (Api.VerRev v) =
  infoBadge "Arch" v.buildArch


osBadge :: forall p i. Api.VerRev -> H.HTML p i
osBadge (Api.VerRev v) =
  infoBadge "OS" v.buildOS


gitReposBadge :: forall p i. Api.VerRev -> H.HTML p i
gitReposBadge (Api.VerRev v) =
  infoBadge "Repos" v.gitStatus


gitCommitsBadge :: forall p i. Api.VerRev -> H.HTML p i
gitCommitsBadge (Api.VerRev v) =
  infoBadge "Commits" v.gitCommitCount


healthBadge :: forall p i. Result3 Api.SystemHealth -> H.HTML p i
healthBadge = case _ of
  Indefinite ->
    control []
  Ok (Api.SystemHealth { system: "Green" }) ->
    greenBadge
  Ok (Api.SystemHealth { system: "Yellow" }) ->
    warningBadge
  Ok _ ->
    failureBadge
  Err _ ->
    haltedBadge


greenBadge :: forall p i. H.HTML p i
greenBadge =
  let c = packLR Tag.Dark $ Tag.CommonColor BC.Success
      t = packLR "System" "green"
  in
  control [ badge c t ]


warningBadge :: forall p i. H.HTML p i
warningBadge =
  let c = packLR Tag.Dark $ Tag.CommonColor BC.Warning
      t = packLR "System" "warning"
  in
  control [ badge c t ]


failureBadge :: forall p i. H.HTML p i
failureBadge =
  let c = packLR Tag.Dark $ Tag.CommonColor BC.Danger
      t = packLR "System" "failure"
  in
  control [ badge c t ]


haltedBadge :: forall p i. H.HTML p i
haltedBadge =
  let c = packLR Tag.Dark $ Tag.CommonColor BC.Danger
      t = packLR "System" "halted"
  in
  control [ badge c t ]
