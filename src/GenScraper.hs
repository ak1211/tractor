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
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeFamilies          #-}
module GenScraper
    ( UnexpectedHTMLException(..)
    , throwScrapingEx
    , PairNV
    , takeNVpairs
    , toPairNV
    , toCheckMark
    , CheckMark(..)
    , InputTag(..)
    , FormTag(..)
    , AnchorTag(..)
    , takeFormTag
    , takeAnchorTag
    , toDecimal
    , toDouble
    ) where
import           Control.Applicative    ((<|>))
import qualified Control.Arrow          as A
import           Control.Exception.Safe
import           Control.Monad          ((>=>))
import qualified Data.ByteString.Char8  as B8
import qualified Data.Maybe             as Mb
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import qualified Data.Text.Read         as Read
import qualified Data.Typeable
import           Text.XML.Cursor        (($//), (&|))
import qualified Text.XML.Cursor        as X

-- |
-- 実行時例外 : 予想外のHTML
newtype UnexpectedHTMLException
    = UnexpectedHTMLException String
    deriving (Data.Typeable.Typeable, Eq, Show)

instance Exception UnexpectedHTMLException

-- |
-- スクレイピングに失敗した場合の例外送出
throwScrapingEx :: MonadThrow m => String -> m a
throwScrapingEx = throwM . UnexpectedHTMLException

--
--
data CheckMark = Checked | Unchecked deriving (Eq, Show)
toCheckMark :: Bool -> CheckMark
toCheckMark True  = Checked
toCheckMark False = Unchecked

-- |
-- input タグの内容
data InputTag = InputTag
    { inputType    :: Maybe T.Text
    , inputName    :: Maybe T.Text
    , inputValue   :: Maybe T.Text
    , inputChecked :: CheckMark
    } deriving (Eq, Show)

--
--
isEmptyInputTag :: InputTag -> Bool
isEmptyInputTag InputTag{..} =
    cond inputType inputName inputValue
    where
    cond Nothing Nothing Nothing = True
    cond _ _ _                   = False

-- |
-- input タグを取り出す
takeInputTags :: X.Cursor -> [InputTag]
takeInputTags cursor =
    filter
    (not . isEmptyInputTag)
    (cursor $// X.laxElement "input" &| pack)
     where
    --
    --
    pack c = InputTag
        { inputType     = go "type" c
        , inputName     = go "name" c
        , inputValue    = go "value" c
        , inputChecked  = toCheckMark . Mb.isJust $ go "checked" c
        }
    --
    --
    go n =
        Mb.listToMaybe . X.laxAttribute n

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
    Mb.listToMaybe . X.laxAttribute "name"
    A.&&&
    Mb.listToMaybe . X.laxAttribute "value"

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
    { formAction   :: T.Text
    , formMethod   :: Maybe T.Text
    , formInputTag :: [InputTag]
    } deriving (Eq, Show)

-- |
-- anchor タグの内容
data AnchorTag = AnchorTag
    { aText :: T.Text
    , aHref :: T.Text
    } deriving (Eq, Show)

-- |
-- form タグの内容を取り出す
takeFormTag :: X.Cursor -> [FormTag]
takeFormTag c =
    map pack $ X.laxAttribute "action" c
    where
    pack :: T.Text -> FormTag
    pack a = FormTag
        { formAction    = a
        , formMethod    = Mb.listToMaybe $ X.laxAttribute "method" c
        , formInputTag  = takeInputTags c
        }

-- |
-- リンクを取り出す関数
takeAnchorTag :: X.Cursor -> [AnchorTag]
takeAnchorTag cursor =
    concat (cursor $// X.laxElement "a" &| pack)
    where
    --
    --
    pack :: X.Cursor -> [AnchorTag]
    pack c =
        zipWith (\x y -> AnchorTag {aText = x, aHref = y})
        (txt c <|> alt c <|> lost)
        (href c)
    --
    -- href属性を取り出す
    href :: X.Cursor -> [T.Text]
    href = X.laxAttribute "href"
    --
    -- リンクテキストを取り出す
    txt :: X.Cursor -> [T.Text]
    txt c = map T.strip (c $// X.content)
    --
    -- リンク内imgタグのaltを取り出す
    alt :: X.Cursor -> [T.Text]
    alt c = c $// X.laxElement "img" >=> X.laxAttribute "alt"
    --
    -- リンクテキストが無い場合
    lost = [T.empty]

-- |
--
-- >>> toDecimal "1,000"
-- 1000
-- >>> toDecimal "10,000円"
-- 10000
-- >>> toDecimal "+1,000円"
-- 1000
-- >>> toDecimal "-5,000円"
-- -5000
-- >>> toDecimal "-5,a00"
-- -5
-- >>> toDecimal "-a00" :: Maybe Int
-- Nothing
toDecimal :: (MonadThrow m, Integral a) => T.Text -> m a
toDecimal t =
    case Read.signed Read.decimal $ T.filter (/= ',') t of
    Right v -> pure $ fst v
    Left v  -> throwString $ "toDecimal: " ++ v

-- |
--
-- >>> toDouble "10,000.34"
-- 10000.34
-- >>> toDouble "+1,000.45"
-- 1000.45
-- >>> toDouble "-5,000"
-- -5000.0
-- >>> toDouble "-5,a00"
-- -5.0
-- >>> toDouble "-a00" :: Maybe Double
-- Nothing
toDouble :: MonadThrow m => T.Text -> m Double
toDouble t =
    case Read.signed Read.double $ T.filter (/= ',') t of
    Right v -> pure $ fst v
    Left v  -> throwString $ "toDouble: " ++ v

