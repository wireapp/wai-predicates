module Main where

import Test.Tasty
import qualified Tests.Data.Predicate as Predicate

main :: IO ()
main = defaultMain $ testGroup "Tests" [ Predicate.tests ]
