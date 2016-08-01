{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Predicate.Content
    ( contentType
    , module Network.Wai.Predicate.MediaType
    ) where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Monoid hiding (All)
import Data.Predicate
import Data.Singletons.TypeLits (Symbol)
import Data.Maybe
import Network.Wai.Predicate.Error
import Network.Wai.Predicate.MediaType
import Network.Wai.Predicate.Request
import Network.Wai.Predicate.Utility

import qualified Network.Wai.Predicate.Parser.MediaType as M

contentType :: HasHeaders r
            => ByteString
            -> ByteString
            -> Predicate r Error (Media (t :: Symbol) (s :: Symbol))
contentType t s r =
    let mtypes = M.readMediaTypes "content-type" r in
    case findContentType t s mtypes of
        m:_ -> Okay (1.0 - mediaQuality m) m
        []  -> Fail (e415 & setMessage msg)
  where
    msg = "Expected 'Content-Type: " <> t <> "/" <> s <> "'."

findContentType :: ByteString -> ByteString -> [M.MediaType] -> [Media t s]
findContentType t s = mapMaybe (\m -> do
    let mt = M.medType m
        ms = M.medSubtype m
    guard ((t == "*" || t == mt) && (s == "*" || s == ms))
    return $ Media mt ms (quality t s) (M.medParams m))
  where
    quality "*" "*" = 0
    quality "*"  _  = 0.2
    quality  _  "*" = 0.5
    quality  _   _  = 1.0
