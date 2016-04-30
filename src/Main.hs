{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Control.Exception
import Text.Read (readMaybe)
import qualified Data.Text.Lazy as TL
import Data.Default.Class
import Data.Foldable (forM_)

import HubicAuthServer

newtype UsageError = UsageError String deriving Show

instance Exception UsageError where
#if MIN_VERSION_base(4,8,0)
    displayException (UsageError s) = s
#endif

main :: IO ()
main =
    catch run $ \e -> do
#if MIN_VERSION_base(4,8,0)
        hPutStrLn stderr (displayException e)
#else
        hPutStrLn stderr (show e)
#endif
        forM_ (fromException e :: Maybe UsageError) $ \_ ->
            hPutStrLn stderr "Try 'cp --help' for more information."
        exitFailure

run :: IO ()
run = do
    args <- getArgs
    let (fs, extraArgs, errs) = getOpt Permute optDescrs args
    let (help, opts) = foldl (flip id) (False, def) fs
    if  | (err : _) <- errs -> throwIO $ UsageError err
        | (arg : _) <- extraArgs ->
            throwIO $ UsageError $ "unrecognized argument `" ++ arg ++ "'"
        | help -> do
            progName <- getProgName
            let usage1 = "Usage: " ++ progName ++ " [OPTION]...\n"
            putStr $ usageInfo usage1 optDescrs
        | otherwise -> runHubicAuthServer opts

optDescrs
    :: [OptDescr ((Bool, HubicAuthServerOpts) -> (Bool, HubicAuthServerOpts))]
optDescrs =
    [ Option "p" ["port"]
        (ReqArg setPort "NUM")
        ("port to listen on (default: " ++ show (optPort def) ++ ")")
    , Option "a" ["address"]
        (ReqArg (\s (help, opts) -> (help, opts {optAddr = s})) "HOST")
        ("address to listen on (default: " ++ optAddr def ++ ")")
    , Option "t" ["max-cache-ttl"]
        (ReqArg setCacheTTL "NUM")
        ("maximum time (in minutes) to cache auth tokens (default: "
           ++ show (optCacheTTL def `quot` usecInMin) ++ ")")
    , Option "u" ["base-url"]
             (ReqArg setURL "URL")
             "the URL that this server is reachable at if reverse proxied"
    , Option "h" ["help"]
        (NoArg $ \(_, opts) -> (True, opts))
        "display this help and exit"
    ]
  where
    setPort s (help, opts) =
        case readMaybe s of
            Nothing -> throw $ UsageError $ "invalid port number `" ++ s ++ "'"
            Just n -> (help, opts {optPort = n})
    setCacheTTL s (help, opts) =
        case readMaybe s of
            Nothing -> throw $ UsageError $ "invalid max-cache-ttl `" ++ s ++ "'"
            Just m | t <- m * usecInMin ->
                let t' = if 0 <= t && t <= toInteger (maxBound :: Int)
                             then fromInteger t
                             else maxBound
                in  (help, opts {optCacheTTL = t'})
    setURL s (help, opts)
        | TL.isSuffixOf "/" s' = (help, opts {optURL = Just s'})
        | otherwise = throw $ UsageError "the URL must end with a /"
      where
        s' = TL.pack s
    usecInMin :: Integral a => a
    usecInMin = 60 * 1000000
