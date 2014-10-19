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
import           Data.Text.Case.Types
import qualified Data.Text.Internal.Fusion             as Fusion
import           Data.Text.Internal.Fusion.CaseMapping (upperMapping, lowerMapping)
import           Data.Text.Internal.Fusion.Common
import           Data.Text.Internal.Fusion.Types
import qualified Data.Text.Internal.Lazy.Fusion        as LFusion
import qualified Data.Text.Lazy                        as LText

takeWord :: Stream Char -> Stream Char
takeWord = transform (const Done) yield . tokenise
{-# INLINE [0] takeWord #-}

dropWord :: Stream Char -> Stream Char
dropWord (tokenise -> Stream next0 s0 len) = Stream next (True :*: s0) len
  where
    next (skip :*: s) =
        case next0 s of
            Done       -> Done
            Skip    s' -> Skip (skip :*: s')
            Yield t s' ->
                case t of
                    B '\0'     -> Skip    (False :*: s')
                    B _ | skip -> Skip    (False :*: s')
                    B c        -> Yield c (False :*: s')
                    _   | skip -> Skip    (skip  :*: s')
                    U c        -> Yield c (skip  :*: s')
                    L c        -> Yield c (skip  :*: s')
{-# INLINE [0] dropWord #-}

toTitle :: Stream Char -> Stream Char
toTitle = mapHead toUpper . transformWith (yield ' ') yield lower . tokenise
{-# INLINE [0] toTitle #-}

toCamel :: Stream Char -> Stream Char
toCamel = mapHead toLower . transformWith skip' upper lower . tokenise
{-# INLINE [0] toCamel #-}

toPascal :: Stream Char -> Stream Char
toPascal = mapHead toUpper . transformWith skip' upper lower . tokenise
{-# INLINE [0] toPascal #-}

toSnake :: Stream Char -> Stream Char
toSnake = transform (yield '_') lower . tokenise
{-# INLINE [0] toSnake #-}

toSpinal :: Stream Char -> Stream Char
toSpinal = transform (yield '-') lower . tokenise
{-# INLINE [0] toSpinal #-}

toTrain :: Stream Char -> Stream Char
toTrain = mapHead toUpper . transformWith (yield '-') upper lower . tokenise
{-# INLINE [0] toTrain #-}

strict :: (Stream Char -> Stream Char) -> Text -> Text
strict f t = Fusion.unstream (f (Fusion.stream t))
{-# INLINE [0] strict #-}

lazy :: (Stream Char -> Stream Char) -> LText.Text -> LText.Text
lazy f t = LFusion.unstream (f (LFusion.stream t))
{-# INLINE [0] lazy #-}

skip' :: forall s. s -> Step (CC s) Char
skip' s = Skip (CC s '\0' '\0')

yield, upper, lower :: forall s. Char -> s -> Step (CC s) Char
yield !c s = Yield c (CC s '\0' '\0')
upper !c s = upperMapping c s
lower !c s = lowerMapping c s

-- | Step across word boundaries using a custom action, and transform
-- both subsequent uppercase and lowercase characters uniformly.
--
-- /See:/ 'transformWith'
transform :: (forall s. s -> Step (CC s) Char)         -- ^ Boundary action.
          -> (forall s. Char -> s -> Step (CC s) Char) -- ^ Character mapping.
          -> Stream Token                              -- ^ Input stream.
          -> Stream Char
transform s m = transformWith s m m
{-# INLINE [0] transform #-}

-- | Step across word boundaries using a custom action, and transform
-- subsequent characters after the word boundary is encountered with a mapping
-- depending on case.
transformWith :: (forall s. s -> Step (CC s) Char)         -- ^ Boundary action.
              -> (forall s. Char -> s -> Step (CC s) Char) -- ^ Boundary mapping.
              -> (forall s. Char -> s -> Step (CC s) Char) -- ^ Subsequent character mapping.
              -> Stream Token                              -- ^ Input stream.
              -> Stream Char
transformWith md mu mc (Stream next0 s0 len) =
    -- HINT: len incorrect when the boundary replacement yields a char.
    Stream next (CC (False :*: False :*: s0) '\0' '\0') len
  where
    next (CC (up :*: prev :*: s) '\0' _) =
        case next0 s of
            Done       -> Done
            Skip    s' -> Skip (CC (up :*: prev :*: s') '\0' '\0')
            Yield t s' ->
                case t of
                    B  _        -> md   (False :*: True  :*: s')
                    U  c | prev -> mu c (True  :*: False :*: s')
                    L  c | prev -> mu c (False :*: False :*: s')
                    U  c | up   -> mu c (True  :*: False :*: s')
                    U  c        -> mc c (True  :*: False :*: s')
                    L  c        -> mc c (False :*: False :*: s')

    next (CC s a b) = Yield a (CC s b '\0')
{-# INLINE [0] transformWith #-}

-- | A token representing characters and boundaries in a stream.
data Token
    = B  {-# UNPACK #-} !Char -- ^ Word boundary.
    | U  {-# UNPACK #-} !Char -- ^ Upper case character.
    | L  {-# UNPACK #-} !Char -- ^ Lower case character.
      deriving (Show)

-- | Tokenise a character stream using the default 'isBoundary' predicate.
--
-- /See:/ 'tokeniseWith'
tokenise :: Stream Char -- ^ Input stream.
         -> Stream Token
tokenise = tokeniseWith isBoundary
{-# INLINE [0] tokenise #-}

-- | Tokenise a character stream using a custom boundary predicate.
tokeniseWith :: (Char -> Bool) -- ^ Boundary predicate.
             -> Stream Char    -- ^ Input stream.
             -> Stream Token
tokeniseWith f (Stream next0 s0 len) =
    -- HINT: len incorrect if there are adjacent boundaries, which are skipped.
    Stream next (CC (True :*: False :*: False :*: s0) '\0' '\0') len
  where
    next (CC (start :*: up :*: prev :*: s) '\0' _) =
        case next0 s of
            Done               -> Done
            Skip    s'         -> Skip (CC (start :*: up :*: prev :*: s') '\0' '\0')
            Yield c s'
                | not b, start -> push
                | up           -> push
                | b, prev      -> Skip (step start)
                | otherwise    -> push
              where
                push | b         = Yield (B c)    (step False)
                     | u, skip   = Yield (U c)    (step False)
                     | u         = Yield (B '\0') (CC (False :*: u :*: b :*: s') c '\0')
                     | otherwise = Yield (L c)    (step False)

                step p = CC (p :*: u :*: b :*: s') '\0' '\0'

                skip = up || start || prev

                b = f c
                u = Char.isUpper c

    next (CC s a b) = Yield (U a) (CC s b '\0')
{-# INLINE [0] tokeniseWith #-}

mapHead :: (Stream Char -> Stream Char) -> Stream Char -> Stream Char
mapHead f s = maybe s (\(x, s') -> f (singleton x) `append` s') (uncons s)
{-# INLINE [0] mapHead #-}
