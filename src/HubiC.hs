{-# LANGUAGE OverloadedStrings #-}

module HubiC
    ( getEndpoint
    , getRefreshToken
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Base64 as B64
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as Map
import Network.HTTP.Client
import Network.HTTP.Types.Header (ResponseHeaders)
import Data.Time
import Data.Monoid

import Util

getEndpoint
    :: Manager
    -> ByteString
    -> ByteString
    -> ByteString
    -> IO (Text, Text, Int)
getEndpoint man client_id client_secret refresh_token = do
    access_token <- getAccessToken man client_id client_secret refresh_token
    let auth = "Bearer " <> access_token
    req0 <- parseUrl "https://api.hubic.com/1.0/account/credentials"
    let req = req0
            { requestHeaders = ("Authorization", auth) : requestHeaders req0
            }
    (res, hdrs) <- httpJSON req man
    let token = lookupObj "token" res
        endpoint = lookupObj "endpoint" res
        expires0 = lookupObj "expires" res
    expires <- parseTimeM True defaultTimeLocale "%FT%T%z" expires0
    now <-
        case fmap C8.unpack (lookup "Date" hdrs)
                >>= parseTimeM True defaultTimeLocale "%a, %d %b %Y %T %Z" of
            Just t -> return t
            Nothing -> getCurrentTime
    let expires_in = fromInteger $ min (toInteger (maxBound :: Int)) $
                    (* 1000000) $ truncate $ diffUTCTime expires now
    return (token, endpoint, expires_in)

getAccessToken
    :: Manager
    -> ByteString
    -> ByteString
    -> ByteString
    -> IO ByteString
getAccessToken man client_id client_secret refresh_token = do
    res <- getTokenGen man client_id client_secret body
    return (T.encodeUtf8 $ lookupObj "access_token" res)
  where
    body =
        [ ("refresh_token", refresh_token)
        , ("grant_type", "refresh_token")
        ]

getRefreshToken
    :: Manager
    -> ByteString
    -> ByteString
    -> ByteString
    -> ByteString
    -> IO ByteString
getRefreshToken man client_id client_secret code redirect_uri = do
    res <- getTokenGen man client_id client_secret body
    return (T.encodeUtf8 $ lookupObj "refresh_token" res)
  where
    body =
        [ ("code", code)
        , ("redirect_uri", redirect_uri)
        , ("grant_type", "authorization_code")
        ]

getTokenGen
    :: Manager
    -> ByteString
    -> ByteString
    -> [(ByteString, ByteString)]
    -> IO A.Object
getTokenGen man client_id client_secret body = do
    req0 <- parseUrl "https://api.hubic.com/oauth/token/"
    let req = urlEncodedBody body req0
            { requestHeaders = ("Authorization", auth) : requestHeaders req0
            }
    (obj, _) <- httpJSON req man
    return obj
  where
    auth = "Basic " <> B64.encode (mconcat [client_id, ":", client_secret])

httpJSON :: Request -> Manager -> IO (A.Object, ResponseHeaders)
httpJSON req man = do
    resp <- httpLbs req man
    let ct = lookup "Content-Type" (responseHeaders resp)
    case ct of
        Nothing -> errorIO "No Content-Type header found"
        Just s0 | s <- C8.takeWhile (/= ';') s0
                , s == "application/json" ->
            return ()
        Just s ->
            errorIO $ "Expected an application/json but got " ++ C8.unpack s
    case A.decode (responseBody resp) of
        Just obj -> return (obj, responseHeaders resp)
        Nothing -> errorIO "Response is not a valid JSON object"

lookupObj :: A.FromJSON a => Text -> A.Object -> a
lookupObj name obj =
    case Map.lookup name obj of
        Nothing -> error $ "JSON object does not contain the \""
                        ++ T.unpack name ++ "\" key"
        Just val0 ->
            case A.fromJSON val0 of
                A.Success val -> val
                A.Error msg -> error $ "Could not decode JSON value: " ++ msg
