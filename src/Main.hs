{-# LANGUAGE MultiWayIf #-}

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Control.Exception
import Text.Read

import HttpServer

data Options = Options
    { help :: Bool
    , port :: String
    }

defaultOptions :: Options
defaultOptions = Options
    { help = False
    , port = "8080"
    }

optDescrs :: [OptDescr (Options -> Options)]
optDescrs =
    [ Option "p"
             ["port"]
             (ReqArg (\s opts -> opts {port = s}) "NUM")
             ("port to listen on (default: " ++ port defaultOptions ++ ")")
    , Option "h"
             ["help"]
             (NoArg $ \opts -> opts {help = True})
             "display this help and exit"
    ]

main :: IO ()
main = do
    args <- getArgs
    let (fs, extraArgs, errs) = getOpt Permute optDescrs args
    let opts = foldl (flip id) defaultOptions fs
    if  | (err : _) <- errs -> do
            hPutStr stderr err
            exitFailure
        | (arg : _) <- extraArgs -> do
            hPutStrLn stderr $ "unrecognized argument `" ++ arg ++ "'"
            exitFailure
        | help opts -> do
            progName <- getProgName
            let usage1 = "Usage: " ++ progName ++ " [-p NUM]\n"
            putStr $ usageInfo usage1 optDescrs
        | otherwise -> do
            case readMaybe (port opts) of
                Nothing -> do
                    hPutStrLn stderr $ "invalid port number `" ++ port opts ++ "'"
                    exitFailure
                Just p ->
                    catch (runHttpServer p) $ \e -> do
                        hPutStrLn stderr $ show (e :: SomeException)
                        exitFailure
