module NoLit
where

import Text.Regex.Posix
import Data.Maybe
import qualified Data.Map as M

import Control.Exception as CE

import Debug.Trace

type Chunk = [String]
type FileContent = String

data TangledFile = TangledFile {
    file :: FilePath,
    contents :: String
} deriving (Eq,Show)
    

generateSourceFiles :: String -> M.Map String [String] -> FileContent
generateSourceFiles root chunks = unlines $ concatMap expandLines (chunks M.! root)
    where
        expandLines :: String -> [String]
        expandLines line = 
            let header = getHeader $ matchLabel line
            in case header of
                Just label -> concatMap expandLines (chunks M.! label)
                Nothing    -> [line]

-- Matches the regular expression looking for a header start of line
matchHeader header = (header =~ "^<([a-zA-Z\\/\\.]+|\\*)>=\\s*$") :: (String,String,String,[String])
matchLabel label = (label =~ "^(\\s*)<([a-zA-Z\\/\\.]+)>\\s*$") :: (String,String,String,[String])

getHeader :: (String,String,String,[String]) -> Maybe String
getHeader (_,_,_,[]) = Nothing
getHeader (_,_,_,m:ms) = Just m

getLabel :: (String,String,String,[String]) -> Maybe String
getLabel (_,_,_,s:l:ls) = Just l
getLabel _ = Nothing

createChunksMap :: [[String]] -> M.Map String [String]
createChunksMap chunks = M.fromList $ map (\c -> (extractName $ head c, tail c)) chunks
    where
        extractName :: String -> String
        extractName header = chunkName $ matchHeader header
        chunkName (_,_,_,[])   = error "header should be detected"
        chunkName (_,_,_,m:ms) = m

slice :: [String] -> [Chunk]
slice [] = []
slice (l:ls)
    | l =~ pattern :: Bool = let (chunk, rest) = span (\x -> not (x =~ pattern :: Bool)) ls
                             in chunk : slice (tail rest)
    | otherwise = slice ls
    where
        pattern = "^-{4,}$"
 
-- The first level of the <*>= tag is intended for file definition
tangle :: String -> [TangledFile]
tangle content = map (\x -> TangledFile x (generateSourceFiles x theMap)) files
    where 
        root = case M.lookup "*" theMap of
            Just x -> x
            Nothing -> error "Malformed file, missing the root \"*\" element"

        files = mapMaybe (getLabel . matchLabel) root
        theMap = createChunksMap.slice $ lines content

