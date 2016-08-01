{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Predicate.Accept
    ( accept
    , module Network.Wai.Predicate.MediaType
    ) where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Monoid hiding (All)
import Data.Maybe
import Data.Predicate
import Data.Singletons.TypeLits (Symbol)
import Network.Wai.Predicate.Error
import Network.Wai.Predicate.Request
import Network.Wai.Predicate.MediaType
import Network.Wai.Predicate.Utility

import qualified Network.Wai.Predicate.Parser.MediaType as M

accept :: HasHeaders r
       => ByteString
       -> ByteString
       -> Predicate r Error (Media (t :: Symbol) (s :: Symbol))
accept t s r =
    let mtypes = M.readMediaTypes "accept" r in
    if null mtypes
        then return (Media t s 1.0 [])
        else case findMediaType t s mtypes of
            m:_ -> Okay (1.0 - mediaQuality m) m
            []  -> Fail (e406 & setMessage msg)
      where
        msg = "Expected 'Accept: " <> t <> "/" <> s <> "'."

findMediaType :: ByteString -> ByteString -> [M.MediaType] -> [Media t s]
findMediaType t s = mapMaybe (\m -> do
    let mt = M.medType m
        ms = M.medSubtype m
    guard ((mt == "*" || t == mt) && (ms == "*" || s == ms))
    return $ Media t s (M.medQuality m) (M.medParams m))
