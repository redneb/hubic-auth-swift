{-# LANGUAGE MultiWayIf #-}

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Control.Exception
import Control.DeepSeq (force)

import Options
import HttpServer

main :: IO ()
main = do
    args <- getArgs
    let (fs, extraArgs, errs) = getOpt Permute optDescrs args
    let opts = force $ foldl (flip id) defaultOptions fs
    if  | (err : _) <- errs -> do
            hPutStr stderr err
            exitFailure
        | (arg : _) <- extraArgs -> do
            hPutStrLn stderr $ "unrecognized argument `" ++ arg ++ "'"
            exitFailure
        | optHelp opts -> do
            progName <- getProgName
            let usage1 = "Usage: " ++ progName ++ " [-p NUM]\n"
            putStr $ usageInfo usage1 optDescrs
        | otherwise ->
            catch (runHttpServer opts) $ \e -> do
                hPutStrLn stderr $ show (e :: SomeException)
                exitFailure
