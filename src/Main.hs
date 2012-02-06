module Main where

import System.Environment
import Text.Regex.Posix

slice :: [String] -> [[String]]
slice [] = []
slice (l:ls)
    | l =~ "-{4,}" :: Bool = let (chunk, rest) = span (\x -> not (x =~ "-{4,}" :: Bool)) ls
                             in chunk : slice (tail rest)
    | otherwise = slice ls
       
main = do
    args <- getArgs
    nwFile <- readFile (args !! 0)
    putStrLn "Hola"
