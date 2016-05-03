{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module HubicAuthServer
    ( HubicAuthServerOpts(..)
    , HubicAuthServerError(..)
    , runHubicAuthServer
    ) where

import Network.Wai.Handler.Warp (defaultSettings, setPort, setHost,
                                 setBeforeMainLoop, setOnException,
                                 defaultShouldDisplayException)
import Network.Wai (modifyResponse, mapResponseHeaders)
import Web.Scotty hiding (Options)
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
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.HashMap.Strict as Map
import System.IO (stderr)
import Text.Hastache (hastacheStr, MuType(MuVariable), defaultConfig)
import Text.Hastache.Context (mkStrContext)
import Text.Read (readMaybe)
import Data.FileEmbed
import Data.Default.Class
import System.Random (randomIO)
import Paths_hubic_auth_swift (version)
import Data.Version (showVersion)
import Data.String
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
#if !MIN_VERSION_base(4,8,0)
import Data.Word (Word)
import Data.Monoid (Monoid(..))
#endif

import Hubic

type Cache = Map.HashMap (ByteString, ByteString, ByteString)
                         (Maybe (Text, Text))

type Registrations = Map.HashMap Word (ByteString, ByteString, ByteString)

data HubicAuthServerOpts = HubicAuthServerOpts
    { optPort :: Int
    , optAddr :: String
    , optCacheTTL :: Int -- in microseconds
    , optURL :: Maybe TL.Text -- must end with a /
    , optOnReady :: IO ()
    , optErrorLogger :: Text -> IO ()
    }

instance Default HubicAuthServerOpts where
    def = HubicAuthServerOpts
        { optPort = 8080
        , optAddr = "127.0.0.1"
        , optCacheTTL = 30 * usecInMin
        , optURL = Nothing
        , optOnReady = return ()
        , optErrorLogger = T.hPutStrLn stderr
        }
      where
        usecInMin = 60 * 1000000

newtype HubicAuthServerError = HubicAuthServerError String deriving (Eq, Show)

instance Exception HubicAuthServerError where
#if MIN_VERSION_base(4,8,0)
    displayException (HubicAuthServerError msg) = msg
#endif

hubicASError :: MonadIO m => String -> m a
hubicASError = liftIO . throwIO . HubicAuthServerError

runHubicAuthServer :: HubicAuthServerOpts -> IO ()
runHubicAuthServer opts = do
    man <- newManager tlsManagerSettings
    cache <- newTVarIO mempty
    registrations <- newTVarIO (mempty :: Registrations)
    let warpSettings =
            setHost (fromString $ optAddr opts) $
            setPort (optPort opts) $
            setBeforeMainLoop (optOnReady opts) $
            setOnException onExceptionHandler $
                defaultSettings
    scottyOpts def {verbose = 0, settings = warpSettings} $ do
        middleware $ modifyResponse $ mapResponseHeaders $ \hdrs ->
            serverHdr : filter ((/= "Server") . fst) hdrs
        get "/v1.0"      $ handleAuth man cache (optCacheTTL opts)
        get "/auth/v1.0" $ handleAuth man cache (optCacheTTL opts)
        get "/" $ do
            code  <- param "code" `rescue` const next
            sessionId <- do
                state <- param "state"
                case readMaybe state of
                    Just val -> return val
                    Nothing -> hubicASError $
                        state ++ " is not a valid integer"
            (client_id, client_secret, redirect_uri) <- do
                regs <- liftIO $ readTVarIO registrations
                case Map.lookup sessionId regs of
                    Just t -> return t
                    Nothing -> hubicASError "Could not find registration data"
            refresh_token <- liftIO $ getRefreshToken man
                client_id client_secret code redirect_uri
            let muCtx "user" = MuVariable $ T.decodeLatin1 $
                    mconcat [client_id, ":", client_secret]
                muCtx "key" = MuVariable $ T.decodeLatin1 refresh_token
                muCtx "auth_uri" = MuVariable $ T.decodeLatin1 $
                    mconcat [redirect_uri, "auth/v1.0"]
                muCtx s = error $ "unknown parameter: " ++ s
            s <- hastacheStr defaultConfig tmplSuccess (mkStrContext muCtx)
            html s
        get "/" $ do
            host <- headerReq "Host"
            let redirect_uri =
                    fromMaybe (mconcat ["http://", host, "/"]) (optURL opts)
            s <- hastacheStr defaultConfig tmplStart $
                mkStrContext $ \"redirect_uri" -> MuVariable redirect_uri
            html s
        post "/" $ do
            client_id     <- param "client_id"
            client_secret <- param "client_secret"
            redirect_uri  <- param "redirect_uri"
            when (TL.null client_id) $
                hubicASError "client_id must not be empty"
            when (TL.null client_secret) $
                hubicASError "client_secret must not be empty"
            when (TL.null redirect_uri ) $
                hubicASError "redirect_uri must not be empty"
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
    onExceptionHandler _ e =
        when (defaultShouldDisplayException e)
            $ optErrorLogger opts $ T.pack $ displayException e

handleAuth :: Manager -> TVar Cache -> Int -> ActionM ()
handleAuth man cache cacheTTL = do
    user <- headerBS "X-Auth-User"
    refresh_token <- headerBS "X-Auth-Key"
    let (client_id, client_secret0) = C8.break (== ':') user
    when (C8.null client_secret0) $ hubicASError "Invalid username format"
    let client_secret = C8.drop 1 client_secret0
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
                    let expires_in' = min cacheTTL $ max 0 $
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
        Nothing -> hubicASError $ "Header " ++ TL.unpack name ++ " not found"
        Just s -> return s

urlEncodeTL :: TL.Text -> TL.Text
urlEncodeTL =
    TL.fromStrict . T.decodeUtf8 . urlEncode True . BL.toStrict . TL.encodeUtf8

tmplStart :: Text
tmplStart = T.decodeUtf8 $(embedFile "html/start.html")

tmplSuccess :: Text
tmplSuccess = T.decodeUtf8 $(embedFile "html/success.html")

#if !MIN_VERSION_base(4,8,0)
displayException :: Exception e => e -> String
displayException = show
#endif
