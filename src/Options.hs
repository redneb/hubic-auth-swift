{-# LANGUAGE DeriveGeneric #-}

module Options
    ( Options(..)
    , defaultOptions
    , optDescrs
    ) where

import System.Console.GetOpt
import Text.Read
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

data Options = Options
    { optHelp :: Bool
    , optPort :: Int
    , optAddr :: String
    }
  deriving (Generic)

instance NFData Options

defaultOptions :: Options
defaultOptions = Options
    { optHelp = False
    , optPort = 8080
    , optAddr = "*"
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
    , Option "h"
             ["help"]
             (NoArg $ \opts -> opts {optHelp = True})
             "display this help and exit"
    ]
  where
    parsePort s =
        case readMaybe s of
            Just n -> n
            Nothing -> error $ "invalid port number `" ++ s ++ "'"
