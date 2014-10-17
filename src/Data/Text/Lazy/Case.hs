-- Module      : Data.Text.Lazy.Case
-- Copyright   : (c) 2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Data.Text.Lazy.Case where

import           Data.Text.Buildable
import           Data.Text.Case.Fusion  (lazy)
import qualified Data.Text.Case.Fusion  as Fusion
import           Data.Text.Case.Types
import           Data.Text.Lazy         (Text)
import           Data.Text.Lazy.Builder (toLazyText)

lowerFirst :: Text -> Text
lowerFirst = lazy Fusion.lowerFirst

upperFirst :: Text -> Text
upperFirst = lazy Fusion.upperFirst

toOrdinal :: Integral a => a -> Text
toOrdinal = toLazyText . build . Ordinal

toCamel :: Text -> Text
toCamel = lazy Fusion.toCamel

toPascal :: Text -> Text
toPascal = lazy Fusion.toPascal

toSnake :: Text -> Text
toSnake = lazy Fusion.toSnake

toSpinal :: Text -> Text
toSpinal = lazy Fusion.toSpinal

toTrain :: Text -> Text
toTrain = lazy Fusion.toTrain

toHuman :: Text -> Text
toHuman = lazy Fusion.toHuman
