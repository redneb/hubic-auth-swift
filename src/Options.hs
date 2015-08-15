{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Options
    ( Options(..)
    , defaultOptions
    , optDescrs
    ) where

import System.Console.GetOpt
import Text.Read
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import qualified Data.Text.Lazy as TL

data Options = Options
    { optHelp :: Bool
    , optPort :: Int
    , optAddr :: String
    , optCacheTTL :: Int -- in microseconds
    , optURL :: Maybe TL.Text -- must end with a /
    }
  deriving (Generic)

instance NFData Options

defaultOptions :: Options
defaultOptions = Options
    { optHelp = False
    , optPort = 8080
    , optAddr = "127.0.0.1"
    , optCacheTTL = 30 * usecInMin
    , optURL = Nothing
    }

optDescrs :: [OptDescr (Options -> Options)]
optDescrs =
    [ Option "p"
             ["port"]
             (ReqArg (\s opts -> opts {optPort = parsePort s}) "NUM")
             ("port to listen on (default: "
                ++ show (optPort defaultOptions) ++ ")")
    , Option "a"
             ["address"]
             (ReqArg (\s opts -> opts {optAddr = s}) "HOST")
             ("address to listen on (default: "
                ++ optAddr defaultOptions ++ ")")
    , Option "t"
             ["max-cache-ttl"]
             (ReqArg (\s opts -> opts {optCacheTTL = parseCacheTTL s}) "NUM")
             ("maximum time (in minutes) to cache auth tokens (default: "
                ++ show (optCacheTTL defaultOptions `quot` usecInMin) ++ ")")
    , Option "u"
             ["base-url"]
             (ReqArg (\s opts -> opts {optURL = Just (parseURL s)}) "URL")
             "the URL that this server is reachable at if reverse proxied"
    , Option "h"
             ["help"]
             (NoArg $ \opts -> opts {optHelp = True})
             "display this help and exit"
    ]
  where
    parsePort s =
        case readMaybe s of
            Nothing -> error $ "invalid port number `" ++ s ++ "'"
            Just n -> n
    parseCacheTTL s =
        case readMaybe s of
            Nothing -> error $ "invalid max-cache-ttl `" ++ s ++ "'"
            Just m | t <- m * usecInMin ->
                if 0 <= t && t <= toInteger (maxBound :: Int)
                    then fromInteger t
                    else maxBound
    parseURL s
        | TL.isSuffixOf "/" s' = s'
        | otherwise = error "the URL must end with a /"
      where
        s' = TL.pack s

usecInMin :: Integral a => a
usecInMin = 60 * 1000000
