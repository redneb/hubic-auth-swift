{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Hubic
    ( HubicError(..)
    , getEndpoint
    , getRefreshToken
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Base64 as B64
import qualified Data.Aeson as A
import Data.Aeson (FromJSON, withObject, (.:))
import Network.HTTP.Client
import Network.HTTP.Types.Header (ResponseHeaders)
import Data.Time
import Data.Monoid
import Control.Exception

newtype HubicError = HubicError String deriving (Eq, Show)

instance Exception HubicError where
#if MIN_VERSION_base(4,8,0)
    displayException (HubicError msg) = msg
#endif

hubicError :: String -> IO a
hubicError = throwIO . HubicError

data GetCredentials = GetCredentials Text Text UTCTime

instance FromJSON GetCredentials where
    parseJSON v = flip (withObject "object") v $ \o ->
        GetCredentials
            <$> o .: "token"
            <*> o .: "endpoint"
            <*> timeP o
      where
        timeP o = do
            s <- o .: "expires"
            case parseTimeM True defaultTimeLocale "%FT%T%z" s of
                Just t -> return t
                Nothing -> fail $ "Invalid timestamp: " ++ s

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
    (GetCredentials token endpoint expires, hdrs) <- httpJSON req man
    now <-
        case fmap C8.unpack (lookup "Date" hdrs)
                >>= parseTimeM True defaultTimeLocale "%a, %d %b %Y %T %Z" of
            Just t -> return t
            Nothing -> getCurrentTime
    let expires_in = fromInteger $ min (toInteger (maxBound :: Int)) $
                    (* 1000000) $ truncate $ diffUTCTime expires now
    return (token, endpoint, expires_in)

newtype GetAccessToken = GetAccessToken ByteString

instance FromJSON GetAccessToken where
    parseJSON v = flip (withObject "object") v $ \o ->
        GetAccessToken . T.encodeUtf8 <$> o .: "access_token"

getAccessToken
    :: Manager
    -> ByteString
    -> ByteString
    -> ByteString
    -> IO ByteString
getAccessToken man client_id client_secret refresh_token = do
    GetAccessToken res <- getTokenGen man client_id client_secret body
    return res
  where
    body =
        [ ("refresh_token", refresh_token)
        , ("grant_type", "refresh_token")
        ]

newtype GetRefreshToken = GetRefreshToken ByteString

instance FromJSON GetRefreshToken where
    parseJSON v = flip (withObject "object") v $ \o ->
        GetRefreshToken . T.encodeUtf8 <$> o .: "refresh_token"

getRefreshToken
    :: Manager
    -> ByteString
    -> ByteString
    -> ByteString
    -> ByteString
    -> IO ByteString
getRefreshToken man client_id client_secret code redirect_uri = do
    GetRefreshToken res <- getTokenGen man client_id client_secret body
    return res
  where
    body =
        [ ("code", code)
        , ("redirect_uri", redirect_uri)
        , ("grant_type", "authorization_code")
        ]

getTokenGen
    :: FromJSON a
    => Manager
    -> ByteString
    -> ByteString
    -> [(ByteString, ByteString)]
    -> IO a
getTokenGen man client_id client_secret body = do
    req0 <- parseUrl "https://api.hubic.com/oauth/token/"
    let req = urlEncodedBody body req0
            { requestHeaders = ("Authorization", auth) : requestHeaders req0
            }
    (obj, _) <- httpJSON req man
    return obj
  where
    auth = "Basic " <> B64.encode (mconcat [client_id, ":", client_secret])

httpJSON :: FromJSON a => Request -> Manager -> IO (a, ResponseHeaders)
httpJSON req man = do
    resp <- httpLbs req man
    let ct = lookup "Content-Type" (responseHeaders resp)
    case ct of
        Nothing -> hubicError "No Content-Type header found"
        Just s0 | s <- C8.takeWhile (/= ';') s0
                , s == "application/json" ->
            return ()
        Just s ->
            hubicError $ "Expected an application/json but got " ++ C8.unpack s
    case A.decode (responseBody resp) of
        Just obj -> return (obj, responseHeaders resp)
        Nothing -> hubicError "Response is not a valid JSON object"
