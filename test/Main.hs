{-# LANGUAGE OverloadedStrings #-}

-- Module      : Main
-- Copyright   : (c) 2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Main (main) where

import Control.Applicative
import Data.List           (sort)
import Data.Text           (Text, unpack)
import Data.Text.Case
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "tests"
    [ test "toHuman" toHuman
        [ "This is AOK"              @: "This is AOK"
        , "A-Slightly_Longer String" @: "A slightly longer string"
        ]
    ]

test :: TestName -> a -> [a -> TestTree] -> TestTree
test n f = testGroup n . map ($ f)

(@:) :: (Show a, Eq a) => Text -> a -> (Text -> a) -> TestTree
x @: y = \f -> testCase (unpack x) (y @=? f x)
