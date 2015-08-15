{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module HttpServer
    ( runHttpServer
    ) where

import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.Wai (modifyResponse, mapResponseHeaders)
import Web.Scotty
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (urlEncode)
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C8
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.HashMap.Strict as Map
import Text.Hastache (hastacheStr, MuType(MuVariable), defaultConfig)
import Text.Hastache.Context (mkStrContext)
import Text.Read (readMaybe)
import Data.FileEmbed
import Data.Default.Class
import System.Random (randomIO)
import Paths_hubic_auth_swift (version)
import Data.Version (showVersion)
import Data.Monoid ((<>))
#if !MIN_VERSION_base(4,8,0)
import Data.Word (Word)
import Data.Monoid (Monoid(..))
#endif

import Util
import HubiC

type Cache = Map.HashMap (ByteString, ByteString, ByteString)
                         (Maybe (Text, Text))

type Registrations = Map.HashMap Word (ByteString, ByteString, ByteString)

runHttpServer :: Int -> IO ()
runHttpServer port = do
    man <- newManager tlsManagerSettings
    cache <- newTVarIO mempty
    registrations <- newTVarIO (mempty :: Registrations)
    scottyOpts def {verbose = 0, settings = setPort port defaultSettings} $ do
        middleware $ modifyResponse $ mapResponseHeaders $ \hdrs ->
            serverHdr : filter ((/= "Server") . fst) hdrs
        get "/v1.0"      $ handleAuth man cache
        get "/auth/v1.0" $ handleAuth man cache
        get "/" $ do
            code  <- param "code" `rescue` const next
            sessionId <- do
                state <- param "state"
                case readMaybe state of
                    Just val -> return val
                    Nothing -> errorIO $ state ++ " is not a valid integer"
            (client_id, client_secret, redirect_uri) <- do
                regs <- liftIO $ readTVarIO registrations
                case Map.lookup sessionId regs of
                    Just t -> return t
                    Nothing -> errorIO "Could not find registration data"
            refresh_token <- liftIO $ getRefreshToken man
                client_id client_secret code redirect_uri
            let muCtx "user" = MuVariable $ T.decodeLatin1 client_id
                muCtx "key" = MuVariable $ T.decodeLatin1 $
                    mconcat [client_secret, ":", refresh_token]
                muCtx "auth_uri" = MuVariable $ T.decodeLatin1 $
                    mconcat [redirect_uri, "auth/v1.0"]
                muCtx s = error $ "unknown parameter: " ++ s
            s <- hastacheStr defaultConfig tmplSuccess (mkStrContext muCtx)
            html s
        get "/" $ do
            host <- headerReq "Host"
            let redirect_uri = mconcat ["http://", host, "/"]
            s <- hastacheStr defaultConfig tmplStart $
                mkStrContext $ \"redirect_uri" -> MuVariable redirect_uri
            html s
        post "/" $ do
            client_id     <- param "client_id"
            client_secret <- param "client_secret"
            redirect_uri  <- param "redirect_uri"
            when (TL.null client_id) $
                errorIO "client_id must not be empty"
            when (TL.null client_secret) $
                errorIO "client_secret must not be empty"
            when (TL.null redirect_uri ) $
                errorIO "redirect_uri must not be empty"
            sessionId <- liftIO randomIO
            liftIO $ atomically $
                modifyTVar registrations $ Map.insert sessionId
                    ( T.encodeUtf8 $ TL.toStrict client_id
                    , T.encodeUtf8 $ TL.toStrict client_secret
                    , T.encodeUtf8 $ TL.toStrict redirect_uri
                    )
            liftIO $ void $ forkFinally (threadDelay $ 300 * 1000000) $ \_ ->
                atomically $ modifyTVar registrations $ Map.delete sessionId
            redirect $ mconcat
                [ "https://api.hubic.com/oauth/auth/?"
                , "client_id=", urlEncodeTL client_id
                , "&redirect_uri=", urlEncodeTL redirect_uri
                , "&scope=credentials.r"
                , "&response_type=code"
                , "&state=", urlEncodeTL $ TL.pack $ show sessionId
                ]
        notFound $
            html $ mconcat ["<div>There's nothing here</div><div>You may want to start from <a href=\"/\">here</a>.</div>"]
  where
    serverHdr =
        ("Server", "hubic-auth-swift/" <> C8.pack (showVersion version))

handleAuth :: Manager -> TVar Cache -> ActionM ()
handleAuth man cache = do
    client_id <- headerBS "X-Auth-User"
    key <- headerBS "X-Auth-Key"
    let (client_secret, refresh_token0) = C8.break (== ':') key
    when (C8.null refresh_token0) $ error "Invalid password format"
    let refresh_token = C8.drop 1 refresh_token0
    let triplet = (client_id, client_secret, refresh_token)
    r <- liftIO $ atomically $ do
        cacheV <- readTVar cache
        case Map.lookup triplet cacheV of
            Just (Just r) -> return (Just r)
            Just Nothing -> retry
            Nothing -> do
                writeTVar cache (Map.insert triplet Nothing cacheV)
                return Nothing
    (token, endpoint) <-
        case r of
            Just r' -> return r'
            Nothing -> liftIO $ do
                let remove = atomically $ modifyTVar cache $ Map.delete triplet
                flip onException remove $ do
                    (token, endpoint, expires_in) <-
                        getEndpoint man client_id client_secret refresh_token
                    atomically $ modifyTVar cache $
                        Map.insert triplet $ Just (token, endpoint)
                    let expires_in' = min 1800000000 $ max 0 $
                                        expires_in - 60000000
                    void $ forkFinally
                        (threadDelay expires_in')
                        (const remove)
                    return (token, endpoint)
    addHeader "X-Auth-Token"  (TL.fromStrict token)
    addHeader "X-Storage-Url" (TL.fromStrict endpoint)
    html mempty

headerBS :: TL.Text -> ActionM ByteString
headerBS = liftM (BL.toStrict . TL.encodeUtf8) . headerReq

headerReq :: TL.Text -> ActionM TL.Text
headerReq name = do
    r <- header name
    case r of
        Nothing -> errorIO $ "Header " ++ TL.unpack name ++ " not found"
        Just s -> return s

urlEncodeTL :: TL.Text -> TL.Text
urlEncodeTL =
    TL.fromStrict . T.decodeUtf8 . urlEncode True . BL.toStrict . TL.encodeUtf8

tmplStart :: Text
tmplStart = T.decodeUtf8 $(embedFile "html/start.html")

tmplSuccess :: Text
tmplSuccess = T.decodeUtf8 $(embedFile "html/success.html")
