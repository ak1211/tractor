{-
    This file is part of Tractor.

    Tractor is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Tractor is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with Tractor.  If not, see <http://www.gnu.org/licenses/>.
-}
{- |
Module      :  GenScraper
Description :  generic scraper
Copyright   :  (c) 2016 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  POSIX

スクレイピングするモジュールです。

>>> :set -XOverloadedStrings
-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module GenScraper
    ( TextAndHref
    , PairNV
    , takeNVpairs
    , toPairNV
    , InputTag(..)
    , FormTag(..)
    , AnchorTag(..)
    , takeFormTag
    ) where
import qualified Control.Arrow         as A
import qualified Data.ByteString.Char8 as B8
import qualified Data.Maybe            as Maybe
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import           Text.XML.Cursor       (($//), (&|))
import qualified Text.XML.Cursor       as X

-- |
-- input タグの内容
data InputTag = InputTag
    { inputType :: Maybe T.Text
    , inputName :: Maybe T.Text
    , inputValue:: Maybe T.Text
    } deriving (Eq, Show)

--
--
isEmptyInputTag :: InputTag -> Bool
isEmptyInputTag =
    (==) $ InputTag Nothing Nothing Nothing

-- |
-- input タグを取り出す
takeInputTags :: X.Cursor -> [InputTag]
takeInputTags cursor =
    filter
    (not . isEmptyInputTag)
    (cursor $// X.anyElement &| pack)
     where
    --
    --
    pack c = InputTag
        { inputType = go "type" c
        , inputName = go "name" c
        , inputValue= go "value" c
        }
    --
    --
    go n =
        Maybe.listToMaybe . X.attribute n

-- |
-- link text and href pair
type TextAndHref = (T.Text, T.Text)

-- |
-- name and value pair
type PairNV a = (Maybe a, Maybe a)

--
--
toPairNV :: InputTag -> PairNV B8.ByteString
toPairNV t =
    (TE.encodeUtf8 <$> inputName t, TE.encodeUtf8 <$> inputValue t)

--
--
isEmptyPair :: (Eq a) => (Maybe a, Maybe a) -> Bool
isEmptyPair = (==) (Nothing, Nothing)

-- |
-- nameとvalue属性をペアで取り出す
attrNameAndValue :: X.Cursor -> PairNV T.Text
attrNameAndValue =
    Maybe.listToMaybe . X.attribute "name"
    A.&&&
    Maybe.listToMaybe . X.attribute "value"

-- |
-- name, value属性ペアを取り出す
takeNVpairs :: X.Cursor -> [PairNV T.Text]
takeNVpairs c =
    filter
    (not . isEmptyPair)
    (c $// X.anyElement &| attrNameAndValue)

-- |
-- form タグの内容
data FormTag = FormTag
    { formAction    :: T.Text
    , formMethod    :: Maybe T.Text
    , formInputTag  :: [InputTag]
    } deriving (Eq, Show)

-- |
-- anchor タグの内容
data AnchorTag = AnchorTag
    { anchorText :: T.Text
    , anchorHref :: Maybe T.Text
    , anchorAlt  :: Maybe T.Text
    } deriving (Eq, Show)

-- |
-- form タグの内容を取り出す
takeFormTag :: X.Cursor -> [FormTag]
takeFormTag c =
    map pack $ X.attribute "action" c
    where
    pack :: T.Text -> FormTag
    pack a = FormTag
        { formAction    = a
        , formMethod    = Maybe.listToMaybe $ X.attribute "method" c
        , formInputTag  = takeInputTags c
        }


