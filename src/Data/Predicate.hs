-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeOperators #-}

module Data.Predicate
    ( Predicate
    , Result (..)
    , constant
    , failure
    , true
    , false
    , and
    , or
    , orElse
    , (:::) (..)
    , (.&.)
    , (.|.)
    , (|||)
    , apply
    , opt
    , def
    , mapResult
    , mapOkay
    , mapFail
    , result
    ) where

import Control.Applicative
import Control.Monad
import Prelude hiding (and, or)

data Result f t
    = Fail !f
    | Okay !Double !t
    deriving (Eq, Ord, Show)

instance Functor (Result f) where
    fmap f (Okay d x) = Okay d (f x)
    fmap _ (Fail   x) = Fail x

instance Applicative (Result f) where
    pure  = return
    (<*>) = ap

instance Monad (Result f) where
    return           = Okay 0
    (Okay _ x) >>= k = k x
    (Fail   x) >>= _ = Fail x

result :: (f -> a) -> (Double -> t -> a) -> Result f t -> a
result f _ (Fail   x) = f x
result _ g (Okay d x) = g d x

type Predicate a f t = a -> Result f t

constant :: t -> Predicate a f t
constant t _ = return t

true :: Predicate a f ()
true = constant ()

failure :: f -> Predicate a f t
failure f _ = Fail f

false :: Predicate a () t
false = failure ()

data a ::: b = a ::: b deriving (Eq, Show)

and, (.&.) :: Predicate a f t -> Predicate a f t' -> Predicate a f (t ::: t')
and f g x = f x `cmp` g x
  where
    cmp (Okay d y) (Okay w z) = Okay (d + w) (y ::: z)
    cmp (Okay _ _) (Fail   y) = Fail y
    cmp (Fail   y) _          = Fail y

or, (.|.) :: Predicate a f t -> Predicate a f t -> Predicate a f t
or f g x = f x `cmp` g x
  where
    cmp a@(Okay d _) b@(Okay w _)  = if w < d then b else a
    cmp a@(Okay _ _)   (Fail _)    = a
    cmp (Fail _)     b@(Okay _ _)  = b
    cmp (Fail _)     b@(Fail _)    = b

orElse, (|||) :: Predicate a f t -> Predicate a f t' -> Predicate a f (Either t t')
orElse f g x = f x `cmp` g x
  where
    cmp (Okay d y) (Okay w z) = if w < d then Okay w (Right z) else Okay d (Left y)
    cmp (Okay d y) (Fail   _) = Okay d (Left y)
    cmp (Fail   _) (Okay d y) = Okay d (Right y)
    cmp (Fail   _) (Fail   y) = Fail y

(.&.) = and
(.|.) = or
(|||) = orElse

apply :: Predicate a f t -> a -> Result f t
apply = ($)

mapResult :: (Result f t -> Result f' t') -> Predicate a f t -> Predicate a f' t'
mapResult f p = f . p

mapOkay :: (t -> Result f t') -> Predicate a f t -> Predicate a f t'
mapOkay f p a =
    case p a of
        Okay _ x -> f x
        Fail   x -> Fail x

mapFail :: (f -> Result f' t) -> Predicate a f t -> Predicate a f' t
mapFail f p a =
    case p a of
        Fail   x -> f x
        Okay d x -> Okay d x

opt :: Predicate a f t -> Predicate a f (Maybe t)
opt = mapResult (result (const $ return Nothing) (\d x -> Okay d (Just x)))

def :: t -> Predicate a f t -> Predicate a f t
def t = mapResult (result (const $ return t) Okay)

