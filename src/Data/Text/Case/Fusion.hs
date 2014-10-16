{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}

-- Module      : Data.Text.Case.Fusion
-- Copyright   : (c) 2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Data.Text.Case.Fusion
    ( lowerHead
    , upperHead

    , toCamel
    , toPascal
    , toSnake
    , toSpinal
    , toTrain
    , toHuman

    , recase
    , split
    ) where

import qualified Data.Char                             as Char
import           Data.Text.Internal.Fusion.CaseMapping
import           Data.Text.Internal.Fusion.Common
import           Data.Text.Internal.Fusion.Types

lowerHead :: Stream Char -> Stream Char
lowerHead = first toLower

upperHead :: Stream Char -> Stream Char
upperHead = first toUpper

toCamel :: Stream Char -> Stream Char
toCamel = first toLower . recase

toPascal :: Stream Char -> Stream Char
toPascal = first toUpper . recase

toSnake :: Stream Char -> Stream Char
toSnake = split Char.toLower '_'

toSpinal :: Stream Char -> Stream Char
toSpinal = split Char.toLower '-'

toTrain :: Stream Char -> Stream Char
toTrain = split Char.toUpper '-'

toHuman :: Stream Char -> Stream Char
toHuman = upperHead . split id ' '

recase :: Stream Char -> Stream Char
recase (Stream next0 s0 len) =
    Stream next (CC (False :*: s0) '\0' '\0') len
  where
    next (CC (p :*: s) '\0' _) = case next0 s of
        Done            -> Done
        Skip s'         -> Skip (CC (p :*: s') '\0' '\0')
        Yield c s'
            | p         -> upperMapping c (b :*: s')
            | b         -> Skip (CC (b :*: s') '\0' '\0')
            | otherwise -> Yield c (CC (p :*: s') '\0' '\0')
          where
            b = boundary c

    next (CC s a b) = Yield a (CC s b '\0')
{-# INLINE recase #-}

split :: (Char -> Char) -> Char -> Stream Char -> Stream Char
split f !x (Stream next0 s0 len) =
    Stream next (CC (True :*: False :*: s0) '\0' '\0') len
  where
    next (CC (d :*: p :*: s) '\0' _) = case next0 s of
        Done                        -> Done
        Skip s'                     -> Skip (step s' p '\0')
        Yield c s'
            | d                     -> Yield (f c) (step s' False '\0')
            | p && b                -> Yield c (step s' b '\0')
            | p                     -> lowerMapping c (False :*: b :*: s')
            | b                     -> Yield x (step s' b '\0')
            | not d, Char.isUpper c -> Yield x (step s' b (f c)) -- FIXME: use lowerMapping
            | otherwise             -> Yield c (step s' b '\0')
          where
            b = boundary c

    next (CC s a b) = Yield a (CC s b '\0')

    step s bdry c = CC (False :*: bdry :*: s) c '\0'
{-# INLINE split #-}

boundary :: Char -> Bool
boundary c = Char.isSpace c || c == '-' || c == '_'
{-# INLINE boundary #-}

first :: (Stream Char -> Stream Char) -> Stream Char -> Stream Char
first f s = maybe s (\(x, s') -> f (singleton x) `append` s') (uncons s)
{-# INLINE first #-}
