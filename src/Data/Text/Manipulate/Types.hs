{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Data.Text.Manipulate.Types
-- Copyright   : (c) 2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Data.Text.Manipulate.Types where

import           Control.Monad
import qualified Data.Char           as Char
import           Data.Monoid
import           Data.Text.Buildable

newtype Ordinal a = Ordinal a

instance Integral a => Buildable (Ordinal a) where
    build (Ordinal (toInteger -> n)) = build n <> suf
      where
        suf | x `elem` [11..13] = "th"
            | y == 1            = "st"
            | y == 2            = "nd"
            | y == 3            = "rd"
            | otherwise         = "th"

        (flip mod 100 -> x, flip mod 10 -> y) = join (,) (abs n)

-- | Returns 'True' for any boundary or uppercase character.
isWordBoundary :: Char -> Bool
isWordBoundary c = Char.isUpper c || isBoundary c

-- | Returns 'True' for any boundary character.
isBoundary :: Char -> Bool
isBoundary = not . Char.isAlphaNum
