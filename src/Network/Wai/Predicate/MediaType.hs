{-# LANGUAGE CPP            #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Wai.Predicate.MediaType where

import Data.ByteString (ByteString)
import GHC.TypeLits
import Data.Text.Encoding (encodeUtf8)
import Data.Proxy
import Data.Text

data Media (t :: Symbol) (s :: Symbol) = Media
    { rawType      :: !ByteString
    , rawSubTypes  :: !ByteString
    , mediaQuality :: !Double
    , mediaParams  :: ![(ByteString, ByteString)]
    } deriving (Eq, Show)

mediaType :: forall t s. KnownSymbol t => Media t s -> ByteString
mediaType _m = encodeUtf8 (pack (symbolVal (Proxy @t)))

mediaSubType :: forall t s. KnownSymbol s => Media t s -> ByteString
mediaSubType _m = encodeUtf8 (pack (symbolVal (Proxy @s)))
