-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Predicate.Internal
    ( readValues
    , rqApply
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.From
import Data.List (foldl')
import Data.Predicate
import Data.String (fromString)
import Network.HTTP.Types
import Network.Wai.Predicate.Error
import Network.Wai.Predicate.Request

readValues :: FromByteString a => [ByteString] -> Either ByteString a
readValues = foldl' res (Left "no parse") . map (runParser parser)
  where
    res (Left  _) (Right x) = Right x
    res (Right x) _         = Right x
    res _         (Left  x) = Left (fromString x)

rqApply :: (Req -> [ByteString])
        -> ([ByteString] -> Either ByteString a)
        -> Error
        -> Predicate Req Error a
rqApply f reader e r =
    case f r of
        [] -> Fail e
        vs -> either (Fail . err status400) (Okay 0) $ reader vs
