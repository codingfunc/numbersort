{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception
import Control.Monad
import Data.Function ((&))
import Data.List
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import qualified Data.Vector.Algorithms.Intro as AI
import qualified Data.Vector.Unboxed as VU
import Formatting
import Formatting.Clock
import System.Clock
import System.Environment
import System.IO
import System.Random

{---
    USAGE:
    $ numbersort generate 1000000 /tmp/input.txt
    $ numbersort test-aha /tmp/input.txt /tmp/output.txt
    $ numbersort test-sp /tmp/input.txt /tmp/output.txt
---}
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

    let sortedStrings = lines input
                      & map (read::String->Int)
                      & sort
                      & map show
                      & unlines

    hOutput <- openFile outputFile WriteMode
    hPutStrLn hOutput sortedStrings
    hFlush hOutput

testSP :: FilePath -> FilePath -> IO ()
testSP inputFile outputFile = do
    input <- TIO.readFile inputFile

    let sortedStrings = T.lines input
                      & map parseInt
                      & VU.fromList
                      & VU.modify AI.sort
                      & VU.toList
                      & map (T.pack . show)
                      & T.unlines

    hOutput <- openFile outputFile WriteMode
    TIO.hPutStrLn hOutput sortedStrings
    hFlush hOutput

parseInt :: T.Text -> Int
parseInt t = case TR.decimal t of
               Right (n, _) -> n
               Left _       -> -1  -- no real parsing error handling for now
