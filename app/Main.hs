module Main where
{-# LANGUAGE OverloadedStrings #-}
import System.IO
import System.Environment
import Data.List
--import Criterion
--import Criterion.Main
-- https://chrisdone.com/posts/measuring-duration-in-haskell
import Control.Exception
import Formatting
import Formatting.Clock
import System.Clock

atoi :: [String] -> [Int]
atoi = map read

itoa :: [Int] -> [String]
itoa = map show


main :: IO ()
main = do
        args <- getArgs
        start <- getTime Monotonic
        input <- readFile(args !! 0)
        let sorted = itoa $ sort . atoi $ lines input
        let output  = "/tmp/hsorted-" ++ (show $ length sorted) ++ ".random"
        hOutput <- openFile output WriteMode
        hPutStrLn hOutput $ unlines sorted
        hFlush hOutput
        end <- getTime Monotonic
        putStrLn $ "Created " ++ output
        fprint (timeSpecs) start end


--    writeFile "/tmp/hsorted.random" sorted

--test1 :: IO()
--test1 = do
--    args <- getArgs
--    input <- readFile(args !! 0)
--    writeFile "/tmp/hsorted.random" $ unlines itoa $ sort $ atoi $ lines input



