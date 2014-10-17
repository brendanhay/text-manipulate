{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes   #-}

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
import           Data.Text                             (Text)
import qualified Data.Text.Internal.Fusion             as Fusion
import           Data.Text.Internal.Fusion.CaseMapping (upperMapping, lowerMapping, titleMapping)
import           Data.Text.Internal.Fusion.Common
import           Data.Text.Internal.Fusion.Types
import qualified Data.Text.Internal.Lazy.Fusion        as LFusion
import qualified Data.Text.Lazy                        as LText

-- | O(n) Returns the first word from a stream, or the entire stream itself
-- if no word boundary is encountered.
takeWord :: Stream Char -> Stream Char
takeWord (Stream next0 s0 len) =
    Stream next (True :*: False :*: s0) len -- HINT maybe too high
  where
    next (start :*: upper :*: s) =
        case next0 s of
            Done            -> Done
            Skip s'         -> Skip (start :*: upper :*: s')
            Yield c s'
                | b, start  -> Skip step
                | u, start  -> Yield c step
                | b         -> Done
                | upper     -> Yield c step
                | u         -> Done
                | otherwise -> Yield c step
              where
                step = False :*: u :*: s'

                b = isBoundary c
                u = Char.isUpper c
{-# INLINE takeWord #-}

-- take :: Integral a => a -> Stream Char -> Stream Char
-- take n0 (Stream next0 s0 len) =
--     Stream next (n0 :*: s0) (smaller len (fromIntegral (max 0 n0)))
--   where
--     next (n :*: s)
--         | n <= 0    = Done
--         | otherwise = case next0 s of
--             Done -> Done
--             Skip s' -> Skip (n :*: s')
--             Yield x s' -> Yield x ((n-1) :*: s')

-- stripWord :: Stream Char -> Maybe (Stream Char)
-- stripWord (Stream next0 s0 len) = go
--   where
--     next !s = case next0 s of
--         Done    -> Nothing
--         Skip s' -> next s
--         Yield c s'
--             | isBoundary c -> Just (Stream next s' len)
--             | otherwise    -> 

-- drop :: Integral a => a -> Stream Char -> Stream Char
-- drop n0 (Stream next0 s0 len) =
--     Stream next (J n0 :*: s0) (len - fromIntegral (max 0 n0))
--   where
--     {-# INLINE next #-}
--     next (J n :*: s)
--       | n <= 0    = Skip (N :*: s)
--       | otherwise = case next0 s of
--           Done       -> Done
--           Skip    s' -> Skip (J n    :*: s')
--           Yield _ s' -> Skip (J (n-1) :*: s')
--     next (N :*: s) = case next0 s of
--       Done       -> Done
--       Skip    s' -> Skip    (N :*: s')
--       Yield x s' -> Yield x (N :*: s')
-- {-# INLINE [0] drop #-}

-- uncons (Stream next s0 len) = loop_uncons s0
--     where
--       loop_uncons !s = case next s of
--                          Yield x s1 -> Just (x, Stream next s1 (len-1))
--                          Skip s'    -> loop_uncons s'
--                          Done       -> Nothing
-- {-# INLINE stripWord #-}


lowerFirst :: Stream Char -> Stream Char
lowerFirst = first toLower
{-# INLINE lowerFirst #-}

upperFirst :: Stream Char -> Stream Char
upperFirst = first toUpper
{-# INLINE upperFirst #-}

toCamel :: Stream Char -> Stream Char
toCamel = lowerFirst . normalise isBoundary
{-# INLINE toCamel #-}

toPascal :: Stream Char -> Stream Char
toPascal = upperFirst . normalise isBoundary
{-# INLINE toPascal #-}

toSnake :: Stream Char -> Stream Char
toSnake = transform isBoundary lowerMapping '_'
{-# INLINE toSnake #-}

toSpinal :: Stream Char -> Stream Char
toSpinal = transform isBoundary lowerMapping '-'
{-# INLINE toSpinal #-}

toTrain :: Stream Char -> Stream Char
toTrain = transform isBoundary titleMapping '-'
{-# INLINE toTrain #-}

toHuman :: Stream Char -> Stream Char
toHuman = upperFirst . transform isBoundary lowerMapping ' '
{-# INLINE toHuman #-}

strict :: (Stream Char -> Stream Char) -> Text -> Text
strict f t = Fusion.unstream (f (Fusion.stream t))
{-# INLINE strict #-}

lazy :: (Stream Char -> Stream Char) -> LText.Text -> LText.Text
lazy f t = LFusion.unstream (f (LFusion.stream t))
{-# INLINE lazy #-}

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
                | b         -> Skip    (CC    (b :*: s') '\0' '\0')
                | bdry      -> upperMapping c (b :*: s')
                | otherwise -> Yield c (CC    (b :*: s') '\0' '\0')
              where
                b = f c

    next (CC s a b) = Yield a (CC s b '\0')
{-# INLINE normalise #-}

-- | This is simply a replica of 'CC' from 'Data.Text.Internal.Fusion.Types'
-- which has an extra slot for the delimiter.
data T s = T !s {-# UNPACK #-} !Char {-# UNPACK #-} !Char {-# UNPACK #-} !Char

-- | Replace word boundaries with a specific delimiter, and transform
-- any subsequent valid characters after the word boundary is encountered.
transform :: (Char -> Bool)                            -- ^ Boundary predicate
          -> (forall s. Char -> s -> Step (CC s) Char) -- ^ Char mapping
          -> Char                                      -- ^ Delimiter
          -> Stream Char
          -> Stream Char
transform f m !d (Stream next0 s0 len) =
    Stream next (T (s0 :*: True :*: False :*: False) '\0' '\0' '\0') len
  where
    next (T (s :*: start :*: upper :*: bdry) '\0' '\0' _) =
        case next0 s of
            Done                   -> Done
            Skip s'                -> Skip (step s' upper bdry)
            Yield c s'
                | start            -> push False s' c u b
                | b, bdry          -> Skip (step s' u b)
                | bdry             -> push False s' c u b
                | b                -> Yield d (step s' u b)
                | u, bdry || start -> push False s' c u b
                | u, upper         -> push False s' c u b
                | u                -> push True  s' c u b
                | otherwise        -> Yield c (step s' u b)
              where
                b = f c
                u = Char.isUpper c

    next (T s a b c) = Yield a (T s b c '\0')

    step s upper bdry = T (s :*: False :*: upper :*: bdry) '\0' '\0' '\0'

    push delim s c upper bdry =
        case m c (s :*: False :*: upper :*: bdry) of
            Yield x (CC s' y z)
                | delim     -> Yield d (T s' x y z)
                | otherwise -> Yield x (T s' y z '\0')
            _               -> Yield c (step s upper bdry)
{-# INLINE transform #-}

isBoundary :: Char -> Bool
isBoundary c = Char.isSpace c || c == '-' || c == '_'
{-# INLINE isBoundary #-}

first :: (Stream Char -> Stream Char) -> Stream Char -> Stream Char
first f s = maybe s (\(x, s') -> f (singleton x) `append` s') (uncons s)
{-# INLINE first #-}
