module Main where

import System.Environment
import NoLit
      
main = do
    args <- getArgs
    nwFile <- readFile (args !! 0)
    putStrLn "Hola"
