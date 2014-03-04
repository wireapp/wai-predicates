-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeOperators #-}

module Data.Predicate.Product where

infixr 5 :::

-- | A data-type for combining results of predicate evaluations.
data a ::: b = a ::: b deriving (Eq, Show)

-- | @flip ($)@ - useful in combination with indexed access, e.g.
-- @('x' ::: True ::: False)#_2@ yields @True@.
(#) :: a -> (a -> b) -> b
(#) = flip ($)

hd :: a ::: b -> a
hd (a ::: _) = a
{-# INLINE hd #-}

tl :: a ::: b -> b
tl (_ ::: b) = b
{-# INLINE tl #-}

-----------------------------------------------------------------------------
-- Indexed access (except for last element)

_1 :: a ::: b -> a
_1 = _1'
{-# INLINE _1 #-}

_2 :: a ::: b ::: c -> b
_2 = hd . _2'
{-# INLINE _2 #-}

_3 :: a ::: b ::: c ::: d -> c
_3 = hd . _3'
{-# INLINE _3 #-}

_4 :: a ::: b ::: c ::: d ::: e -> d
_4 = hd . _4'
{-# INLINE _4 #-}

_5 :: a ::: b ::: c ::: d ::: e ::: f -> e
_5 = hd . _5'
{-# INLINE _5 #-}

_6 :: a ::: b ::: c ::: d ::: e ::: f ::: g -> f
_6 = hd . _6'
{-# INLINE _6 #-}

_7 :: a ::: b ::: c ::: d ::: e ::: f ::: g ::: h -> g
_7 = hd . _7'
{-# INLINE _7 #-}

_8 :: a ::: b ::: c ::: d ::: e ::: f ::: g ::: h ::: i -> h
_8 = hd . _8'
{-# INLINE _8 #-}

_9 :: a ::: b ::: c ::: d ::: e ::: f ::: g ::: h ::: i ::: j -> i
_9 = hd . _9'
{-# INLINE _9 #-}

-----------------------------------------------------------------------------
-- Access last element

_1' :: a ::: b -> a
_1' = hd
{-# INLINE _1' #-}

_2' :: a ::: b -> b
_2' = tl
{-# INLINE _2' #-}

_3' :: a ::: b ::: c -> c
_3' = tl . tl
{-# INLINE _3' #-}

_4' :: a ::: b ::: c ::: d -> d
_4' = tl . tl . tl
{-# INLINE _4' #-}

_5' :: a ::: b ::: c ::: d ::: e -> e
_5' = tl . tl . tl . tl
{-# INLINE _5' #-}

_6' :: a ::: b ::: c ::: d ::: e ::: f -> f
_6' = tl . tl . tl . tl . tl
{-# INLINE _6' #-}

_7' :: a ::: b ::: c ::: d ::: e ::: f ::: g -> g
_7' = tl . tl . tl . tl . tl . tl
{-# INLINE _7' #-}

_8' :: a ::: b ::: c ::: d ::: e ::: f ::: g ::: h -> h
_8' = tl . tl . tl . tl . tl . tl . tl
{-# INLINE _8' #-}

_9' :: a ::: b ::: c ::: d ::: e ::: f ::: g ::: h ::: i -> i
_9' = tl . tl . tl . tl . tl . tl . tl . tl
{-# INLINE _9' #-}

