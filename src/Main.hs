module Main where

import System.Environment
import System.Directory
import System.FilePath.Posix
import System.Environment(getArgs)
import System.Console.GetOpt
import System.IO
import System.Exit

import NoLit
      

createFile :: FilePath -> TangledFile -> IO ()
createFile outDir (TangledFile filename content) = do
    createDirectoryIfMissing True outDir
    createDirectoryIfMissing True $ takeDirectory filename
    writeFile (outDir++"/"++filename) content

-- Options
data Options = Options {
    outDir :: String,
    filename :: String
}

defaultOptions = Options {
    outDir = ".",
    filename = ""
}

readFilename :: String -> Options -> IO Options
readFilename arg opt = return opt { filename = arg }

readOutDir :: String -> Options -> IO Options
readOutDir arg opt = return opt { outDir = arg }

-- Option description
options :: [OptDescr (Options -> IO Options)]
options = [ Option ['o'] ["output-dir"] (ReqArg readOutDir "DIR") "Output directory where generated code has to be placed" 
          ]

header = "Usage: main [OPTION...] filename.acd"

exit :: String -> IO a
exit msg = do
    hPutStrLn stderr msg
    exitFailure

 
main = do
    args <- getArgs
    opts <- case getOpt RequireOrder options args of
        (actions, [filename], []) -> (foldl (>>=) (return defaultOptions) actions) >>= (readFilename filename)
        (_      , nonOpts, msgs)   -> exit $ (if not (null nonOpts) 
                                                then "unrecognized arguments: " ++ unwords nonOpts ++ "\n"
                                                else "") 
                                            ++ concat msgs ++ usageInfo header options
 

    let (actions, [filename], msgs) = getOpt RequireOrder options args 
    opts <- foldl (>>=) (return defaultOptions) actions
    putStrLn (outDir opts)
    nwFile <- readFile filename
    mapM (createFile (outDir opts)) $ tangle nwFile
