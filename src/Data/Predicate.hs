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
    , mapR
    , mapO
    , mapF
    , result
    ) where

import Prelude hiding (and, or)

data Result f t
    = Fail f
    | Okay Double t
    deriving (Eq, Ord, Show)

type Predicate a f t = a -> Result f t

constant :: t -> Predicate a f t
constant t _ = Okay 0 t

true :: Predicate a f ()
true = constant ()

failure :: f -> Predicate a f t
failure f _ = Fail f

false :: Predicate a () t
false = failure ()

infixr 5 :::
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

mapR :: (Result f t -> Result f' t') -> Predicate a f t -> Predicate a f' t'
mapR f p = f . p

mapO :: (t -> Result f t') -> Predicate a f t -> Predicate a f t'
mapO f p a =
    case p a of
        Okay _ x -> f x
        Fail   x -> Fail x

mapF :: (f -> Result f' t) -> Predicate a f t -> Predicate a f' t
mapF f p a =
    case p a of
        Fail   x -> f x
        Okay d x -> Okay d x

opt :: Predicate a f t -> Predicate a f (Maybe t)
opt = mapR (result (const $ Okay 0 Nothing) (\d x -> Okay d (Just x)))

def :: t -> Predicate a f t -> Predicate a f t
def t = mapR (result (const $ Okay 0 t) Okay)

result :: (f -> a) -> (Double -> t -> a) -> Result f t -> a
result f _ (Fail   x) = f x
result _ g (Okay d x) = g d x

