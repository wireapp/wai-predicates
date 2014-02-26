-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Predicate
    ( getRequest

    , query
    , hasQuery

    , header
    , hasHeader

    , capture
    , hasCapture

    , param
    , hasParam

    , cookie
    , hasCookie

    , accept
    , contentType
    , module Network.Wai.Predicate.MediaType
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.From
import Data.CaseInsensitive (mk)
import Data.List (find)
import Data.Monoid
import Data.Maybe (isJust)
import Data.Predicate
import Network.HTTP.Types.Status
import Network.Wai.Predicate.Accept
import Network.Wai.Predicate.Content
import Network.Wai.Predicate.Error
import Network.Wai.Predicate.Internal
import Network.Wai.Predicate.MediaType
import Network.Wai.Predicate.Request
import Network.Wai

getRequest :: Predicate Req f Request
getRequest = Okay 0 . toRequest

query :: FromByteString a => ByteString -> Predicate Req Error a
query k = let msg = "Missing query '" <> k <> "'." in
    rqApply (lookupQuery k) readValues (err status400 msg)

hasQuery :: ByteString -> Predicate Req Error ()
hasQuery k r =
    if null (lookupQuery k r)
        then Fail (err status400 ("Missing query '" <> k <> "'."))
        else Okay 0 ()

header :: FromByteString a => ByteString -> Predicate Req Error a
header k = let msg = "Missing header '" <> k <> "'." in
    rqApply (lookupHeader k) readValues (err status400 msg)

hasHeader :: ByteString -> Predicate Req Error ()
hasHeader k r =
    if isJust $ find ((mk k ==) . fst) (headers r)
        then Okay 0 ()
        else Fail (err status400 ("Missing header '" <> k <> "'."))

capture :: FromByteString a => ByteString -> Predicate Req Error a
capture k = let msg = "Missing path parameter '" <> k <> "'." in
    rqApply (lookupCapture k) readValues (err status400 msg)

hasCapture :: ByteString -> Predicate Req Error ()
hasCapture k r =
    if null (lookupCapture k r)
        then Fail (err status400 ("Missing path parameter '" <> k <> "'."))
        else Okay 0 ()

param :: FromByteString a => ByteString -> Predicate Req Error a
param k = query k .|. capture k

hasParam :: ByteString -> Predicate Req Error ()
hasParam k = hasQuery k .|. hasCapture k

cookie :: FromByteString a => ByteString -> Predicate Req Error a
cookie k = let msg = "Missing cookie '" <> k <> "'." in
    rqApply (lookupCookie k) readValues (err status400 msg)

hasCookie :: ByteString -> Predicate Req Error ()
hasCookie k r =
    if null (lookupCookie k r)
        then Fail (err status400 ("Missing cookie '" <> k <> "'."))
        else Okay 0 ()
