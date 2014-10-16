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

import           Data.Text             (Text)
import           Data.Text.Case.Fusion (strict)
import qualified Data.Text.Case.Fusion as Fusion

-- toLower
-- toTitle
-- toUpper
-- takeWord
-- takeAcronym
-- toOrdinal
-- indent

add some unicode tests

maybe have an acronym table like inflector?
affects the streaming .. perhaps applied as after-effect, opt-in

lowerFirst :: Text -> Text
lowerFirst = strict Fusion.lowerFirst

upperFirst :: Text -> Text
upperFirst = strict Fusion.upperFirst

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
