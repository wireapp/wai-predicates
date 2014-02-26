module Main where

import Test.Tasty
import qualified Tests.Data.Predicate as Predicate
import qualified Tests.Wai.Predicate  as WaiPredicate

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ Predicate.tests
    , WaiPredicate.tests
    ]
