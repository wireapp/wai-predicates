{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Predicate.Parser.MediaType
    ( MediaType (..)
    , readMediaTypes
    ) where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import Data.List (sortBy)
import Network.HTTP.Types
import Network.Wai.Predicate.Request

import qualified Data.ByteString.Char8 as C

data MediaType = MediaType
    { medType    :: !ByteString
    , medSubtype :: !ByteString
    , medQuality :: !Double
    , medParams  :: ![(ByteString, ByteString)]
    } deriving (Eq, Show)

readMediaTypes :: (HasHeaders r) => HeaderName -> r -> [MediaType]
readMediaTypes k r =
    sortBy q . concatMap parseMediaTypes $ lookupHeader k r
  where
    q a b = medQuality b `compare` medQuality a

parseMediaTypes :: ByteString -> [MediaType]
parseMediaTypes = either (const []) id . parseOnly mediaTypes

mediaTypes :: Parser [MediaType]
mediaTypes = mediaType `sepBy` char ','

mediaType :: Parser MediaType
mediaType =
    toMediaType <$> trim typ <*> (char '/' *> trim subtyp) <*> params
  where
    toMediaType t s p =
        case lookup "q" p >>= toDouble of
            Just q  -> MediaType t s q (filter ((/= "q") . fst) p)
            Nothing -> MediaType t s 1.0 p

params :: Parser [(ByteString, ByteString)]
params = (trim (char ';') *> (element `sepBy` trim (char ';'))) <|> return []
  where
    element = (,) <$> trim key <*> (char '=' *> trim val)

typ, subtyp, key, val :: Parser ByteString
typ    = takeTill (oneof "/ ")
subtyp = takeTill (oneof ",; ")

key = do
    c <- peekChar
    if c == Just ',' then fail "comma" else takeTill (oneof "= ")

val = takeTill (oneof ",; ")

toDouble :: ByteString -> Maybe Double
toDouble bs = toMaybe (parseOnly double bs)
  where
    toMaybe (Right x) = Just x
    toMaybe (Left  _) = Nothing

spaces :: Parser ()
spaces = skipWhile (== ' ')

trim :: Parser a -> Parser a
trim p = spaces *> p <* spaces

oneof :: ByteString -> Char -> Bool
oneof s c = C.any (== c) s
