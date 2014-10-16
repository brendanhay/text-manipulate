{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}

-- Module      : Data.Text.Case.Fusion
-- Copyright   : (c) 2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Data.Text.Case.Fusion where

import qualified Data.Char                             as Char
import           Data.Text.Internal.Fusion.CaseMapping
import           Data.Text.Internal.Fusion.Common
import           Data.Text.Internal.Fusion.Types
import           Debug.Trace

lowerFirst :: Stream Char -> Stream Char
lowerFirst = first toLower

upperFirst :: Stream Char -> Stream Char
upperFirst = first toUpper

toCamel :: Stream Char -> Stream Char
toCamel = lowerFirst . normalise boundary

toPascal :: Stream Char -> Stream Char
toPascal = upperFirst . normalise boundary

toSnake :: Stream Char -> Stream Char
toSnake = transform boundary Char.toLower '_'

toSpinal :: Stream Char -> Stream Char
toSpinal = transform boundary Char.toLower '-'

toTrain :: Stream Char -> Stream Char
toTrain = transform boundary Char.toUpper '-'

-- toHuman :: Stream Char -> Stream Char
-- toHuman = upperHead . split id ' '

-- | Remove word boundaries and uppercase any subsequent valid characters.
normalise :: (Char -> Bool) -- ^ Boundary predicate
          -> Stream Char
          -> Stream Char
normalise f (Stream next0 s0 len) =
    Stream next (CC (False :*: s0) '\0' '\0') len
  where
    next (CC (bdry :*: s) '\0' _) =
        case next0 s of
            Done            -> Done
            Skip s'         -> Skip    (CC (bdry :*: s') '\0' '\0')
            Yield c s'
                | bdry      -> upperMapping c (b :*: s')
                | b         -> Skip    (CC (b    :*: s') '\0' '\0')
                | otherwise -> Yield c (CC (b    :*: s') '\0' '\0')
              where
                b = f c

    next (CC s a b) = Yield a (CC s b '\0')
{-# INLINE normalise #-}

-- | Replace word boundaries with a specific delimiter, and transform
-- any subsequent valid characters after boundary is encountered.
transform :: (Char -> Bool) -- ^ Boundary predicate
          -> (Char -> Char) -- ^ Char transformer
          -> Char           -- ^ Delimiter
          -> Stream Char
          -> Stream Char
transform f g !x (Stream next0 s0 len) =
    Stream next (step s0 True False) len
  where
    next (CC (upper :*: bdry :*: s) '\0' _) =
        case next0 s of
            Done               -> Done
            Skip s'            -> Skip        (step s' upper bdry)
            Yield c s'
                | bdry, b      -> Skip        (step s' u b)
                | upper, u     -> Yield (g c) (step s' u True)
                | not upper, u -> Yield x     (push s' u b (g c))
                | b            -> Yield x     (step s' u b)
                | upper        -> Yield (g c) (step s' u b)
                | otherwise    -> Yield c     (step s' u b)
              where
                b = f c
                u = Char.isUpper c

    next (CC s a b) = Yield a (CC s b '\0')

    step s upper bdry   = push s upper bdry '\0'
    push s upper bdry c = CC (upper :*: bdry :*: s) c '\0'
{-# INLINE transform #-}

boundary :: Char -> Bool
boundary c = Char.isSpace c || c == '-' || c == '_'
{-# INLINE boundary #-}

first :: (Stream Char -> Stream Char) -> Stream Char -> Stream Char
first f s = maybe s (\(x, s') -> f (singleton x) `append` s') (uncons s)
{-# INLINE first #-}
