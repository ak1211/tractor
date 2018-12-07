module Page
  ( fixedTopNavBar
  , hero
  , icon
  , footer
  , progress
  ) where

import Prelude

import Bulma.Columns.Columns as Columns
import Bulma.Common as BC
import Bulma.Components.Navbar as Navbar
import Bulma.Elements.Button as Button
import Bulma.Elements.Elements as Elements
import Bulma.Elements.Title as Title
import Bulma.Layout.Layout as Layout
import Bulma.Modifiers.Modifiers as Modifiers
import Bulma.Modifiers.Typography as Typo
import CSS (GenericFontFamily(..), borderBottom, borderLeft, fromString, paddingBottom, paddingLeft, paddingRight, paddingTop, px, rem, rgb, solid)
import CSS.Font (fontFamily)
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.NonEmpty (singleton)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Core as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Halogen.Query as HQ
import Route as Route
import Session (Session(..))


-- font family
cursive :: GenericFontFamily
cursive = GenericFontFamily $ fromString "cursive"


-- TOP NAVIVGATION BAR



fixedTopNavBar :: forall p f. HQ.Action f -> Boolean -> Session -> H.HTML p f
fixedTopNavBar toggleAction isActive session =
  HH.nav
    [ HP.class_ <<< HC.ClassName $ BC.runClassNames
      [ Navbar.navbar
      , Navbar.isFixed Navbar.Top
      ]
    , HPA.role "navigation"
    , HP.attr (HC.AttrName "aria-label") "main navigation"
    , CSS.style do
        borderBottom solid (px 1.0) $
        rgb 0xdc 0xdc 0xdc
    ]
    [ divNavBarBrand toggleAction isActive target
    , divNavBarMenu isActive target session
    ]
  where
  target = "thisisNavBar"


divNavBarBrand :: forall p f. HQ.Action f -> Boolean -> String -> H.HTML p f
divNavBarBrand toggleAction isActive target =
  HH.div
    [ HP.class_ <<< HC.ClassName $ BC.runClassName Navbar.navbarBrand
    ]
    [ HH.div
      [ HP.class_ <<< HC.ClassName $ BC.runClassNames
        [ Navbar.navbarItem
        , Typo.isSize Typo.Size5
        , Typo.hasColor Typo.GreyDarker
        ]
      , CSS.style (fontFamily ["Gugi"] $ singleton cursive)
      ]
      [ HH.text "TRACTOR" ]
    , aNavBarBurger
    ]
  where

  aNavBarBurger =
    HH.a
      [ HP.class_ classNavBarBurger
      , HP.attr (HC.AttrName "aria-label") "menu"
      , HP.attr (HC.AttrName "aria-expanded") "false"
      , HP.attr (HC.AttrName "data-target") target
      , HE.onClick (HE.input_ toggleAction)
      ]
      (A.replicate 6 itemNavBarBurger)

  classNavBarBurger =
    case isActive of
      true ->
        [ Navbar.navbarBurger
        , Navbar.isState Navbar.Active
        ]
      false ->
        [ Navbar.navbarBurger
        ]
    # BC.runClassNames
    # HC.ClassName

  itemNavBarBurger =
    HH.span [ HP.attr (HC.AttrName "aria-hidden") "true" ] []


divNavBarMenu :: forall p i. Boolean -> String -> Session -> H.HTML p i
divNavBarMenu isActive target session =
  let appendix = if isActive then [ Navbar.isState Navbar.Active ] else []
  in
  HH.div
    [ HP.class_ <<< HC.ClassName <<< BC.runClassNames <<< (_ <> appendix) $
      [ Navbar.navbarMenu
      ]
    , HP.id_ target
    ]
    [ divNavBarStart 
    , divNavBarEnd
    ]
  where

  divNavBarStart =
    HH.div
      [ HP.class_ <<< HC.ClassName <<< BC.runClassName $ Navbar.navbarStart
      ]
      [ aNavBarItem (Route.href Route.Dashboard) "Dashboard"
      , aNavBarItem (Route.href Route.Upload) "Upload"
      , aNavBarItem (Route.href Route.Portfolio) "Portfolio"
      , aNavBarItem (Route.href Route.Charts) "Charts"
      , aNavBarItem (Route.href Route.AccountBalance) "AccountBalance"
      , aNavBarItem (Route.href Route.ApiDocument) "ApiDocument"
      ]

  aNavBarItem href text =
    HH.a 
      [ HP.class_ <<< HC.ClassName <<< BC.runClassName $ Navbar.navbarItem
      , href
      ]
      [ HH.text text
      ]

  divNavBarEnd =
    HH.div
      [ HP.class_ <<< HC.ClassName <<< BC.runClassName $ Navbar.navbarEnd
      ]
      [ HH.div 
        [ HP.class_ <<< HC.ClassName <<< BC.runClassName $ Navbar.navbarItem
        ]
        [ HH.div 
          [ HP.classes
            [ HC.ClassName "field"
            , HC.ClassName <<< BC.runClassName $ BC.isGrouped
            ]
          ]
          [ alternateButton session
          ]
        ]
      ]


alternateButton :: forall p i. Session -> H.HTML p i
alternateButton = case _ of
  Guest ->
    iconButton Route.Login "fas fa-sign-in-alt" (Modifiers.isColor BC.Warning) "Log in"
  (LoggedIn _) ->
    iconButton Route.Logout "fas fa-sign-out-alt" (Button.isColor Button.Dark) "Log out"
  where

  iconButton route iconName color text =
    HH.p
      [ HP.class_ (HC.ClassName "control")
      ]
      [ HH.a
        [ HP.class_ <<< HC.ClassName $ BC.runClassNames
          [ Button.button
          , Typo.hasWeight Typo.Bold
          , color
          ]
        , Route.href route
        ]
        [ icon iconName
        , HH.span [] []
        , HH.span
          []
          [ HH.text text
          ]
        ]
      ]


icon :: forall p i. String -> H.HTML p i
icon iconName =
  HH.span
    [ HP.class_ (HC.ClassName "icon")
    ]
    [ HH.i
      [ HP.class_ (HC.ClassName iconName)
      ]
      []
    ]


-- FOOTER


footer :: forall p i. H.HTML p i
footer =
  HH.footer
    [ HP.class_ <<< HC.ClassName $ BC.runClassName Layout.footer
    ]
    [ HH.div
      [ HP.class_ <<< HC.ClassName $ BC.runClassName Columns.columns
      ]
      [ viewOnGitHub
      ]
    , copyright
    ]
    

copyright :: forall p i. H.HTML p i
copyright =
  HH.p
    [ HP.class_ <<< HC.ClassName $ BC.runClassNames
        [ Typo.isSize Typo.Size6
        , Typo.hasAlignment Typo.Centered
        ]
    ]
    [ HH.text "The Assets observation application "
    , HH.span
      [ CSS.style (fontFamily ["Gugi"] $ singleton cursive)
      ]
      [ HH.text "TRACTOR"
      ]
    , HH.text "."
    ]


viewOnGitHub :: forall p i. H.HTML p i
viewOnGitHub =
  HH.div
    [ HP.class_ <<< HC.ClassName $ BC.runClassNames
        [ Columns.column
        , Typo.hasAlignment Typo.Centered
        ]
    ]
    [ HH.a
      [ HP.class_ <<< HC.ClassName $ BC.runClassNames
        [ Button.button
        , Button.isColor Button.Dark
        , Button.isStyle Button.Outlined
        , Typo.hasWeight Typo.Bold
        ]
      , HP.href "https://github.com/ak1211/tractor"
      ]
      [ HH.text "fork me on "
      , HH.span [] []
      , icon "fab fa-github"
      ]
    ]


-- HERO


hero :: forall p i. String -> H.HTML p i
hero title =
  HH.section
    [ HP.class_ <<< HC.ClassName $ BC.runClassName Layout.hero
    ]
    [ HH.div
      [ HP.class_ <<< HC.ClassName $ BC.runClassName Layout.heroBody
      ]
      [ HH.div
        [ HP.class_ <<< HC.ClassName $ BC.runClassName Layout.container
        ]
        [
          HH.h1
          [ HP.class_ <<< HC.ClassName $ BC.runClassNames
            [ Title.title
            , Title.isSize BC.Is1
            ]
          , CSS.style do
              paddingTop $ rem 0.8
              paddingBottom $ rem 0.8
              paddingLeft $ rem 1.2
              paddingRight $ rem 1.2
              borderLeft solid (px 6.0) (rgb 0xde 0xb8 0x87)
          ]
          [ HH.text title
          ]
        ]
      ]
    ]


-- PROGRESS


progress :: forall p i. Maybe String -> H.HTML p i
progress maybeVal =
  HH.p
    [ HP.class_ <<< HC.ClassName $ BC.runClassNames
      [ Typo.isSize Typo.Size5
      , Typo.hasAlignment Typo.Centered
      ]
    ]
    [ HH.text "Now Loading..."
    , HH.progress
      ([ HP.class_ <<< HC.ClassName $ BC.runClassNames
        [ Elements.progress
        , Modifiers.isColor BC.Primary
        ]
      , HP.attr (HC.AttrName "max") "100"
      ] <> appendix)
      []
    ]
  where
  appendix = case maybeVal of
    Nothing -> 
      []
    Just v -> 
      [ HP.attr (HC.AttrName "value") v ]
