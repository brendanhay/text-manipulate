{-# LANGUAGE ViewPatterns #-}

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

import           Control.Arrow
import           Data.Maybe
import           Data.Monoid
import           Data.Text             (Text)
import           Data.Text.Buildable
import           Data.Text.Case.Fusion (strict)
import qualified Data.Text.Case.Fusion as Fusion
import           Data.Text.Case.Types
import qualified Data.Text.Lazy        as LText
import qualified Data.Text.Lazy.Case   as LCase

-- Re-exports
-- toLower
-- toTitle
-- toUpper

-- if this used the same rules as 'transform/normalise' an acronym would
-- also be taken as the first word
takeWord :: Text -> Text
takeWord = undefined

dropWord :: Text -> Text
dropWord t = fromMaybe t (stripWord t)

-- | Return the suffix after the first word boundary is encountered. If not
-- boundary is found, then 'Nothing'.
stripWord :: Text -> Maybe Text
stripWord = undefined

-- investiagate how 'Data.Text.splitOn' doesn't copy substrings
-- | Split into a list delimited by word boundaries.
splitWords :: Text -> [Text]
splitWords = undefined

indentLines :: Int -> Text -> Text
indentLines = undefined

-- FIXME: get the upper/lowerMapping working as parameter to transform

-- FIXME: add some unicode tests

-- FIXME: maybe have an acronym table like inflector?
-- affects the streaming .. perhaps applied as after-effect, opt-in
-- upperAcronyms
-- lowerAcronyms

lowerFirst :: Text -> Text
lowerFirst = strict Fusion.lowerFirst

upperFirst :: Text -> Text
upperFirst = strict Fusion.upperFirst

toOrdinal :: Integral a => a -> Text
toOrdinal = LText.toStrict . LCase.toOrdinal

toCamel :: Text -> Text
toCamel = strict Fusion.toCamel

toPascal :: Text -> Text
toPascal = strict Fusion.toPascal

toSnake :: Text -> Text
toSnake = strict Fusion.toSnake

toSpinal :: Text -> Text
toSpinal = strict Fusion.toSpinal

toTrain :: Text -> Text
toTrain = strict Fusion.toTrain

toHuman :: Text -> Text
toHuman = strict Fusion.toHuman
