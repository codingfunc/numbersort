module Main where

--import Lib
--import Data.List
import System.IO
import System.Environment

atoi :: [String] -> [Int]
atoi = map read

main :: IO ()
main = do
     args <- getArgs
     content <- readFile (args !! 0)
     putStrLn content


--    let linesOfFile  = lines content :: Int
--    print



