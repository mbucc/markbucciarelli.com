module Main where

import           Control.Concurrent   (threadDelay)
import           System.Posix.Signals (Handler (CatchInfo, Default), SignalInfo,
                                       installHandler, sigINT, sigTERM,
                                       siginfoError, siginfoSignal)

dump :: SignalInfo -> String
dump si =
    "signal=" ++ (show (siginfoSignal si))


-- Print's but does not stop program
--
catch1 =
    CatchInfo (\x -> putStrLn $ "catch " ++ (show (dump x)))

main :: IO ()
main = do
    --installHandler sigINT (\x -> putStrLn $ "catch " ++ (show x))
    --installHandler sigTERM (\x -> putStrLn $ "catch " ++ (show x))
    installHandler sigINT  catch1 Nothing
    installHandler sigTERM Default Nothing
    loop 0

loop :: Int -> IO ()
loop i = do
    print i
    threadDelay 1000000 {- µs -}
    loop (i + 1)
