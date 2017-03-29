module Main where

import           Control.Concurrent   (threadDelay)
import           System.Posix.Signals (Handler (Default), installHandler,
                                       sigINT, sigTERM)

main :: IO ()
main = do
    --installHandler sigINT (\x -> putStrLn $ "catch " ++ (show x))
    --installHandler sigTERM (\x -> putStrLn $ "catch " ++ (show x))
    installHandler sigINT Default Nothing
    installHandler sigTERM Default Nothing
    loop 0

loop :: Int -> IO ()
loop i = do
    print i
    threadDelay 1000000 {- µs -}
    loop (i + 1)
