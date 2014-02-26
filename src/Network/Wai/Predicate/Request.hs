-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Predicate.Request
    ( Req
    , fromRequest
    , toRequest
    , method
    , headers
    , lookupHeader
    , lookupCapture
    , lookupQuery
    , lookupCookie
    ) where

import Data.ByteString (ByteString)
import Data.CaseInsensitive (mk)
import Data.Maybe (mapMaybe)
import Network.HTTP.Types
import Network.Wai (Request)
import Web.Cookie

import qualified Network.Wai as Wai

data Req = Req
    { captures :: [(ByteString, ByteString)]
    , request  :: Request
    , cookies  :: Cookies
    }

fromRequest :: [(ByteString, ByteString)] -> Request -> Req
fromRequest ca rq =
    Req ca rq (concatMap parseCookies (getHeaders "Cookie" rq))

toRequest :: Req -> Request
toRequest = request

headers :: Req -> RequestHeaders
headers = Wai.requestHeaders . request

method :: Req -> Method
method = Wai.requestMethod . request

lookupHeader :: ByteString -> Req -> [ByteString]
lookupHeader name = getHeaders name . request

lookupCapture :: ByteString -> Req -> [ByteString]
lookupCapture name = map snd . filter ((name ==) . fst) . captures

lookupCookie :: ByteString -> Req -> [ByteString]
lookupCookie name = map snd . filter ((name ==) . fst) . cookies

lookupQuery :: ByteString -> Req -> [ByteString]
lookupQuery name = mapMaybe snd
                 . filter ((name ==) . fst)
                 . Wai.queryString
                 . request

getHeaders :: ByteString -> Wai.Request -> [ByteString]
getHeaders name = map snd . filter ((mk name ==) . fst) . Wai.requestHeaders

