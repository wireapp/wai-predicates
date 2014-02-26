{-# LANGUAGE FlexibleInstances     #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Tests.Data.Predicate (tests) where

import Control.Applicative hiding (Const, empty)
import Data.Predicate
import Test.QuickCheck hiding (Result, (.&.))
import Test.Tasty
import Test.Tasty.QuickCheck hiding ((.&.))

tests :: TestTree
tests = testGroup "Data.Predicate"
    [ testProperty ".&." testAnd
    , testProperty "|||" testOr
    , testProperty ".|." testOr'
    ]

testAnd :: Predicate () Int Char -> Predicate () Int Char -> Bool
testAnd a@(Rand (Okay d x)) b@(Rand (Okay w y)) = apply (a .&. b) () == Okay (d + w) (x ::: y)
testAnd a@(Rand (Okay _ _)) b@(Rand (Fail   y)) = apply (a .&. b) () == Fail y
testAnd a@(Rand (Fail   x)) b@(Rand (Okay _ _)) = apply (a .&. b) () == Fail x
testAnd a@(Rand (Fail   x)) b@(Rand (Fail   _)) = apply (a .&. b) () == Fail x

testOr :: Predicate () Int Char -> Predicate () Int Char -> Bool
testOr a@(Rand (Okay d x)) b@(Rand (Okay e y)) = apply (a ||| b) () == if d <= e then Okay d (Left x) else Okay e (Right y)
testOr a@(Rand (Okay d x)) b@(Rand (Fail   _)) = apply (a ||| b) () == Okay d (Left x)
testOr a@(Rand (Fail   _)) b@(Rand (Okay d y)) = apply (a ||| b) () == Okay d (Right y)
testOr a@(Rand (Fail   _)) b@(Rand (Fail   y)) = apply (a ||| b) () == Fail y

testOr' :: Predicate () Int Char -> Predicate () Int Char -> Bool
testOr' a@(Rand (Okay d x)) b@(Rand (Okay e y)) = apply (a .|. b) () == if d <= e then Okay d x else Okay e y
testOr' a@(Rand (Okay d x)) b@(Rand (Fail   _)) = apply (a .|. b) () == Okay d x
testOr' a@(Rand (Fail   _)) b@(Rand (Okay d y)) = apply (a .|. b) () == Okay d y
testOr' a@(Rand (Fail   _)) b@(Rand (Fail   y)) = apply (a .|. b) () == Fail y

instance Arbitrary (Result Int Char) where
    arbitrary =
        oneof [ Okay <$> (arbitrary :: Gen Double) <*> (arbitrary :: Gen Char)
              , Fail <$> (arbitrary :: Gen Int)
              ]

instance Arbitrary (() -> Result Int Char) where
    arbitrary = (\r _ -> r) <$> (arbitrary :: Gen (Result Int Char))
