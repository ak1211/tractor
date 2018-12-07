module Page.ApiDocument
  ( Query(..)
  , Slot(..)
  , component
  ) where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat (ResponseFormatError(..))
import Api as Api
import AppM (class SessionDSL, getSession)
import Bulma.Common as BC
import Bulma.Elements.Elements as Elements
import Bulma.Elements.Title as Title
import Bulma.Layout.Layout as Layout
import Data.Array as A
import Data.Either (either)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Properties as HP
import Page as Page
import Session (Session(..))
import Text.Markdown.SlamDown as SD
import Text.Markdown.SlamDown.Parser as SDP


type State =
  { navBarActived :: Boolean
  , session :: Session
  , webApiDocument :: Maybe SD.SlamDown
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
    , webApiDocument: Nothing
    }

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    ToggleNavBar next -> do
      H.modify_ \st ->
        st { navBarActived = not st.navBarActived }
      pure next

    Initialize next -> do
      session <- getSession
      res <- H.liftAff $ Api.getWebApiDocument
      let doc = either (handleErr res) identity res.body
      H.modify_
        _ { session = session
          , webApiDocument = toSlamDown doc
        }
      pure next

    Finalize next -> do
      pure next

    where

    handleErr :: forall a. AX.Response a -> ResponseFormatError -> String
    handleErr resp (ResponseFormatError err _) =
      "# API Access Error \n" <> show (resp.status) <> " \n" <> (show err)
      

toSlamDown :: String -> Maybe SD.SlamDown
toSlamDown original =
  SDP.parseMd original
  # either (const Nothing) Just


renderSlamDown :: forall p i. SD.SlamDown -> H.HTML p i
renderSlamDown (SD.SlamDown xs) = 
  HH.div
    [ HP.class_ <<< HC.ClassName $ BC.runClassName Elements.content
    ]
  (A.fromFoldable $ map block xs)
  where

  --
  -- slamdown block element
  --
  block :: SD.Block String -> H.HTML p i
  block =
    case _ of
      SD.Paragraph is -> HH.p_ (A.fromFoldable $ map inline is)
      SD.Header 1 is -> HH.h1 title (A.fromFoldable $ map inline is)
      SD.Header 2 is -> HH.h2 subtitle (A.fromFoldable $ map inline is)
      SD.Header 3 is -> HH.h3_ (A.fromFoldable $ map inline is)
      SD.Header 4 is -> HH.h4_ (A.fromFoldable $ map inline is)
      SD.Header 5 is -> HH.h5_ (A.fromFoldable $ map inline is)
      SD.Header 6 is -> HH.h6_ (A.fromFoldable $ map inline is)
      SD.Header _ is -> HH.h6_ (A.fromFoldable $ map inline is)
      SD.Blockquote bs -> HH.blockquote_ (A.fromFoldable $ map block bs)
      SD.Lst (SD.Bullet _) bss -> HH.ul_ (A.fromFoldable $ map listItem bss)
      SD.Lst (SD.Ordered _) bss -> HH.ol_ (A.fromFoldable $ map listItem bss)
      SD.CodeBlock _ ss -> HH.div_ []
      SD.LinkReference l url -> HH.strong_ [ HH.text "LinkReference is NOT implemented" ]
      SD.Rule -> HH.strong_ [ HH.text "Rule is NOT implemented" ]
    where

    title = 
      [ HP.class_ <<< HC.ClassName $ BC.runClassNames
        [ Title.title
        , Title.isSize BC.Is1
        ]
      ]

    subtitle = 
      [ HP.class_ <<< HC.ClassName $ BC.runClassNames
        [ Title.subtitle
        , Title.isSize BC.Is2
        ]
      ]

    listItem :: List (SD.Block String) -> H.HTML p i
    listItem bs = 
      HH.li_ (A.fromFoldable $ map block bs)

  --
  -- slamdown inline element
  --
  inline :: SD.Inline String -> H.HTML p i
  inline = case _ of
    SD.Str s -> HH.text s
    SD.Entity s -> HH.text s
    SD.Space -> HH.text " "
    SD.SoftBreak -> HH.br_
    SD.LineBreak -> HH.br_
    SD.Emph is -> HH.em_ (A.fromFoldable $ map inline is)
    SD.Strong is -> HH.strong_ (A.fromFoldable $ map inline is)
    SD.Code e s -> HH.code_ [ HH.text s ]
    --
    -- Render without HTML link elements.
    SD.Link is _ -> HH.span_ (A.fromFoldable $ map inline is)
    --
    --
    SD.Image is url -> HH.strong_ [ HH.text "Image is NOT implemented" ]
    SD.FormField l r e -> HH.strong_ [ HH.text "FormField is NOT implemented" ]


-- RENDER


render :: State -> H.ComponentHTML Query
render state =
  HH.div_
    [ Page.fixedTopNavBar ToggleNavBar state.navBarActived state.session
    , Page.hero "API document"
    , HH.section
        [ HP.class_ <<< HC.ClassName $ BC.runClassName Layout.section
        ]
        [ HH.div
          [ HP.class_ <<< HC.ClassName $ BC.runClassName Layout.container
          ]
          contents
        ]
    , Page.footer
    ]
  where

  contents = case state.webApiDocument of
    Nothing ->
      [ Page.progress Nothing ]
    Just doc ->
      [ renderSlamDown doc ]
  