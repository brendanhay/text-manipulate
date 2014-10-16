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

import           Data.Monoid
import           Data.Text        (Text)
import qualified Data.Text        as Text
import           Data.Text.Case
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "tests"
    [ test "lowerFirst" lowerFirst
        [ ""
        , "title cased phrase"
        , "camelCasedPhrase"
        , "pascalCasedPhrase"
        , "snake_cased_phrase"
        , "spinal-cased-phrase"
        , "train-Cased-Phrase"
        , "1-Mixed_string AOK"
        , "a  double--stop__phrase"
        , "hTML5"
        ]

    , test "upperFirst" upperFirst
        [ ""
        , "Title cased phrase"
        , "CamelCasedPhrase"
        , "PascalCasedPhrase"
        , "Snake_cased_phrase"
        , "Spinal-cased-phrase"
        , "Train-Cased-Phrase"
        , "1-Mixed_string AOK"
        , "A  double--stop__phrase"
        , "HTML5"
        ]

    , test "toCamel" toCamel
        [ ""
        , "titleCasedPhrase"
        , "camelCasedPhrase"
        , "pascalCasedPhrase"
        , "snakeCasedPhrase"
        , "spinalCasedPhrase"
        , "trainCasedPhrase"
        , "1MixedStringAOK"
        , "aDoubleStopPhrase"
        , "hTML5"
        ]

    , test "toPascal" toPascal
        [ ""
        , "TitleCasedPhrase"
        , "CamelCasedPhrase"
        , "PascalCasedPhrase"
        , "SnakeCasedPhrase"
        , "SpinalCasedPhrase"
        , "TrainCasedPhrase"
        , "1MixedStringAOK"
        , "ADoubleStopPhrase"
        , "HTML5"
        ]

    , test "toSnake" toSnake
        [ ""
        , "title_cased_phrase"
        , "camel_cased_phrase"
        , "pascal_cased_phrase"
        , "snake_cased_phrase"
        , "spinal_cased_phrase"
        , "train_cased_phrase"
        , "1_mixed_string_aok"
        , "a_double_stop_phrase"
        , "html5"
        ]

    , test "toSpinal" toSpinal
        [ ""
        , "title-cased-phrase"
        , "camel-cased-phrase"
        , "pascal-cased-phrase"
        , "snake-cased-phrase"
        , "spinal-cased-phrase"
        , "train-cased-phrase"
        , "1-mixed-string-aok"
        , "a-double-stop-phrase"
        , "html5"
        ]

    , test "toTrain" toTrain
        [ ""
        , "Title-Cased-Phrase"
        , "Camel-Cased-Phrase"
        , "Pascal-Cased-Phrase"
        , "Snake-Cased-Phrase"
        , "Spinal-Cased-Phrase"
        , "Train-Cased-Phrase"
        , "1-Mixed-String-AOK"
        , "A-Double-Stop-Phrase"
        , "HTML5"
        ]
    ]

examples :: [Text]
examples =
    [ ""
    , "Title cased phrase"
    , "camelCasedPhrase"
    , "PascalCasedPhrase"
    , "snake_cased_phrase"
    , "spinal-cased-phrase"
    , "Train-Cased-Phrase"
    , "1-Mixed_string AOK"
    , "a  double--stop__phrase"
    , "HTML5"
    ]

test :: TestName -> (Text -> Text) -> [Text] -> TestTree
test n f = testGroup n . zipWith (run f) examples

run :: (Text -> Text) -> Text -> Text -> TestTree
run f x y = testCase n (y @=? f x)
  where
    n | Text.null x = "Empty String"
      | otherwise   = Text.unpack (x <> " -> " <> y)
