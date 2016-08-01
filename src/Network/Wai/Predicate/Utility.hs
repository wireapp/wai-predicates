{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Predicate.Utility (readValues, (&)) where

import Data.ByteString (ByteString)
import Data.ByteString.Conversion
import Data.List (foldl')
import Data.String (fromString)

readValues :: FromByteString a => [ByteString] -> Either ByteString a
readValues = foldl' res (Left "no parse") . map (runParser parser)
  where
    res (Left  _) (Right x) = Right x
    res (Right x) _         = Right x
    res _         (Left  x) = Left (fromString x)

infixl 1 &
(&) :: a -> (a -> b) -> b
a & f = f a
{-# INLINE (&) #-}
