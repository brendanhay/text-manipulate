{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Data.Text.Lazy.Case
-- Copyright   : (c) 2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Most of the functions in this module are subject to fusion, meaning that
-- a pipeline of such functions will usually allocate at most one Text value.
module Data.Text.Lazy.Case
    (
    -- * Character manipulation
      lowerHead
    , upperHead
    , mapHead

    -- * Line manpipulation
    , indentLines
    , prependLines

    -- * Subwords
    -- ** Removing words
    , takeWord
    , dropWord
    , stripWord
    -- ** Breaking on words
    , breakWord
    , splitWords

    -- * Acronyms
    , toAcronym

    -- * Ordinals
    , Ordinal (..)
    , toOrdinal

    -- * Casing
    , toTitle
    , toCamel
    , toPascal
    , toSnake
    , toSpinal
    , toTrain
    ) where

import qualified Data.Char              as Char
import           Data.List              (intersperse)
import           Data.Monoid
import           Data.Text.Buildable
import           Data.Text.Case.Fusion  (lazy)
import qualified Data.Text.Case.Fusion  as Fusion
import           Data.Text.Case.Types
import           Data.Text.Lazy         (Text)
import qualified Data.Text.Lazy         as LText
import           Data.Text.Lazy.Builder (toLazyText)

-- | Lowercase the first character of a piece of text.
--
-- >>> lowerHead "Title Cased"
-- "title Cased"
lowerHead :: Text -> Text
lowerHead = mapHead Char.toLower

-- | Uppercase the first character of a piece of text.
--
-- >>> upperHead "snake_cased"
-- "Snake_cased"
upperHead :: Text -> Text
upperHead = mapHead Char.toUpper

-- | Apply a function to the first character of a piece of text.
mapHead :: (Char -> Char) -> Text -> Text
mapHead f x =
    case LText.uncons x of
        Just (c, cs) -> LText.singleton (f c) <> cs
        Nothing      -> x

-- | Indent newlines by the given number of spaces.
indentLines :: Int -> Text -> Text
indentLines n = prependLines (LText.replicate (fromIntegral n) " ")

-- | Prepend newlines with the given separator
prependLines :: Text -> Text -> Text
prependLines sep = mappend sep . LText.unlines . intersperse sep . LText.lines

-- | O(n) Returns the first word, or the original text if no word
-- boundary is encountered. /Subject to fusion./
takeWord :: Text -> Text
takeWord = lazy Fusion.takeWord

-- | O(n) Return the suffix after dropping the first word. If no word
-- boundary is encountered, the result will be empty. /Subject to fusion./
dropWord :: Text -> Text
dropWord = lazy Fusion.dropWord

-- | Break a piece of text after the first word boundary is encountered.
--
-- >>> breakWord "PascalCasedVariable"
-- ("Pacal", "CasedVariable")
--
-- >>> breakWord "spinal-cased-variable"
-- ("spinal", "cased-variable")
breakWord :: Text -> (Text, Text)
breakWord x = (takeWord x, dropWord x)

-- | O(n) Return the suffix after removing the first word, or 'Nothing'
-- if no word boundary is encountered.
--
-- >>> stripWord "HTML5Spaghetti"
-- Just "Spaghetti"
--
-- >>> stripWord "noboundaries"
-- Nothing
stripWord :: Text -> Maybe Text
stripWord x
    | LText.length y < LText.length x = Just y
    | otherwise                       = Nothing
  where
    y = dropWord x

-- | O(n) Split into a list of words delimited by boundaries.
--
-- >>> splitWords "SupercaliFrag_ilistic"
-- ["Supercali","Frag","ilistic"]
splitWords :: Text -> [Text]
splitWords = go
  where
    go x = case breakWord x of
        (h, t) | LText.null h -> go t
               | LText.null t -> [h]
               | otherwise    -> h : go t

-- | O(n) Create an adhoc acronym from a piece of cased text.
--
-- >>> toAcronym "AmazonWebServices"
-- Just "AWS"
--
-- >>> toAcronym "Learn-You Some_Haskell"
-- Just "LYSH"
--
-- >>> toAcronym "this_is_all_lowercase"
-- Nothing
toAcronym :: Text -> Maybe Text
toAcronym (LText.filter Char.isUpper -> x)
    | LText.length x > 1 = Just x
    | otherwise          = Nothing

-- | Build an ordinal used to denote the position in an ordered sequence.
-- @toOrdinal == build . Ordinal@.
--
-- >>> toOrdinal (101 :: Int)
-- "101st"
--
-- >>> toOrdinal (12 :: Int)
-- "12th"
toOrdinal :: Integral a => a -> Text
toOrdinal = toLazyText . build . Ordinal

-- | O(n) Convert casing to @Titled Cased Phrase@. /Subject to fusion./
toTitle :: Text -> Text
toTitle = lazy Fusion.toTitle

-- | O(n) Convert casing to @camelCasedPhrase@. /Subject to fusion./
toCamel :: Text -> Text
toCamel = lazy Fusion.toCamel

-- | O(n) Convert casing to @PascalCasePhrase@. /Subject to fusion./
toPascal :: Text -> Text
toPascal = lazy Fusion.toPascal

-- | O(n) Convert casing to @snake_cased_phrase@. /Subject to fusion./
toSnake :: Text -> Text
toSnake = lazy Fusion.toSnake

-- | O(n) Convert casing to @spinal-cased-phrase@. /Subject to fusion./
toSpinal :: Text -> Text
toSpinal = lazy Fusion.toSpinal

-- | O(n) Convert casing to @Train-Cased-Phrase@. /Subject to fusion./
toTrain :: Text -> Text
toTrain = lazy Fusion.toTrain
