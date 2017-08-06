{-# LANGUAGE CPP            #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}

module Network.Wai.Predicate.MediaType where

import Data.ByteString (ByteString)
import Data.Singletons
import Data.Singletons.TypeLits

#if MIN_VERSION_singletons(2,3,0)
import Data.Text.Encoding (encodeUtf8)
#else
import Data.ByteString.Char8 (pack)
#endif

data Media (t :: Symbol) (s :: Symbol) = Media
    { rawType      :: !ByteString
    , rawSubTypes  :: !ByteString
    , mediaQuality :: !Double
    , mediaParams  :: ![(ByteString, ByteString)]
    } deriving (Eq, Show)

mediaType :: KnownSymbol t => Media t s -> ByteString
mediaType m = withSing (f m)
  where
    f :: Media t s -> Sing t -> ByteString
#if MIN_VERSION_singletons(2,3,0)
    f _ t = encodeUtf8 (fromSing t)
#else
    f _ t = pack (fromSing t)
#endif

mediaSubType :: KnownSymbol s => Media t s -> ByteString
mediaSubType m = withSing (f m)
  where
    f :: Media t s -> Sing s -> ByteString
#if MIN_VERSION_singletons(2,3,0)
    f _ s = encodeUtf8 (fromSing s)
#else
    f _ s = pack (fromSing s)
#endif
