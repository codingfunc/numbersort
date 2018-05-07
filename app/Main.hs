{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception
import Control.Monad
import Data.Function ((&))
import Data.List
import Data.Monoid ((<>))
import Formatting
import Formatting.Clock
import System.Clock
import System.Environment
import System.IO
import System.Random

stringsToNumbers :: [String] -> [Int]
stringsToNumbers = map read

numbersToStrings :: [Int] -> [String]
numbersToStrings = map show

main :: IO ()
main = do
    args <- getArgs
    let operation : options = args
    case operation of
      "generate" -> generate options
      "test-aha" -> measure "test-aha" testAha options
      "test-sp" -> measure "test-sp" testSP options
      _ -> error "Incorrect operation syntax!"

generate :: [String] -> IO ()
generate args = do
    let rowCountString : outputFile : _ = args
    let rowCount = read rowCountString :: Int

    putStrLn $ "Generating " <> outputFile <> ":"
    hOutput <- openFile outputFile WriteMode

    n <- take rowCount <$> randomNumbers
    forM_ n $ hPrint hOutput

    hFlush hOutput
    putStrLn $ "Generation of " <> outputFile <> " is completed."

randomNumbers :: IO [Int]
randomNumbers = randomRs (0, 10^9) <$> newStdGen

measure :: String -> (FilePath -> FilePath -> IO ()) -> [String] -> IO ()
measure title benchmark args = do
    let inputFile : outputFile : _ = args
    putStrLn $ "Measuring " <> title <> ":"
    start <- getTime Monotonic
    benchmark inputFile outputFile
    end <- getTime Monotonic
    fprint timeSpecs start end

testAha :: FilePath -> FilePath -> IO ()
testAha inputFile outputFile = do
    input <- readFile inputFile
    hOutput <- openFile outputFile WriteMode

    let sorted = lines input & stringsToNumbers & sort & numbersToStrings & unlines

    hPutStrLn hOutput sorted
    hFlush hOutput

testSP :: FilePath -> FilePath -> IO ()
testSP inputFile outputFile = do
    input <- readFile inputFile
    hOutput <- openFile outputFile WriteMode

    let sorted = input  -- FIXME TODO implement sorting

    hPutStrLn hOutput sorted
    hFlush hOutput
