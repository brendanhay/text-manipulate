-- Module      : Data.Text.Case
-- Copyright   : (c) 2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Data.Text.Case where

import qualified Data.Char                        as Char

import Data.Text                        (Text)
import Data.Text.Case.Fusion
import Data.Text.Internal.Fusion
import Data.Text.Internal.Fusion.Common

-- toOrdinal

-- toHuman :: 
toHuman = strict (upperHead . split id ' ')


--     :: [Word]
-- List of Words, first of which will be capitalized
--                             -> String
-- The humanized output
-- Capitalizes the first word and turns underscores into spaces
--  Like titleize, this is meant for creating pretty output.

-- ]

-- lowerHead :: Text -> Text
-- lowerHead = strict (first toLower)

-- upperHead :: Text -> Text
-- upperHead = strict (first toUpper)

-- toCamel :: Text -> Text
-- toCamel = strict (first toLower . recase)

-- toPascal :: Text -> Text
-- toPascal = strict (first toUpper . recase)

-- toSnake :: Text -> Text
-- toSnake = strict (split (Char.toLower) '_')

strict :: (Stream Char -> Stream Char) -> Text -> Text
strict f = unstream . f . stream
{-# INLINE strict #-}

-- -- some_words
-- toSnake
-- -- SOME_WORDS
-- toScreamingSnake

-- toSpinal
-- -- Train-Case
-- toTrain

-- lowered :: String -> String
-- lowered = map toLower

-- camelise :: [String] -> String
-- camelise (x : xs) = concat (x : map (\(y : ys) -> toUpper y : lowered ys) xs)
-- camelise []       = []

-- cased :: Char -> [String] -> String
-- cased x xs = concat $ intersperse [x] (map lowered xs)

-- uncased :: Char -> String -> [String]
-- uncased x xs = y ++ map tail ys
--   where
--   (y, ys) = splitAt 1 (splitBy (== x) xs)

-- splitBy :: (a -> Bool) -> [a] -> [[a]]
-- splitBy p = groupBy (const (not . p))
