{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
testAnd a b = case (apply a (), apply b ()) of
    (Okay d x, Okay w y) -> apply (a .&. b) () == Okay (d + w) (x ::: y)
    (Okay _ _, Fail   y) -> apply (a .&. b) () == Fail y
    (Fail   x, Okay _ _) -> apply (a .&. b) () == Fail x
    (Fail   x, Fail   _) -> apply (a .&. b) () == Fail x

testOr :: Predicate () Int Char -> Predicate () Int Char -> Bool
testOr a b = case (apply a (), apply b ()) of
    (Okay d x, Okay e y) -> apply (a ||| b) () == if d <= e then Okay d (Left x) else Okay e (Right y)
    (Okay d x, Fail   _) -> apply (a ||| b) () == Okay d (Left x)
    (Fail   _, Okay d y) -> apply (a ||| b) () == Okay d (Right y)
    (Fail   _, Fail   y) -> apply (a ||| b) () == Fail y

testOr' :: Predicate () Int Char -> Predicate () Int Char -> Bool
testOr' a b = case (apply a (), apply b ()) of
    (Okay d x, Okay e y) -> apply (a .|. b) () == if d <= e then Okay d x else Okay e y
    (Okay d x, Fail   _) -> apply (a .|. b) () == Okay d x
    (Fail   _, Okay d y) -> apply (a .|. b) () == Okay d y
    (Fail   _, Fail   y) -> apply (a .|. b) () == Fail y

instance Arbitrary (Result Int Char) where
    arbitrary =
        oneof [ Okay <$> (arbitrary :: Gen Double) <*> (arbitrary :: Gen Char)
              , Fail <$> (arbitrary :: Gen Int)
              ]

instance Arbitrary (Predicate () Int Char) where
    arbitrary = (\r -> const r) <$> (arbitrary :: Gen (Result Int Char))
