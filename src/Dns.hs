module Dns
(
	parse
) where

import System.Directory
import qualified Data.Set as S

dnsTypeProcessors :: [(Char, String -> [String])]
dnsTypeProcessors =
	[('C', processAlias)]

processAlias :: String -> [String]
processAlias line = [first, last]
    where getPart = takeWhile (/= ':')
          dropPart = dropWhile (/= ':')
          first = getPart line
          last = (getPart . tail . dropPart) line

processLine :: String -> Maybe [String]
processLine [] = Nothing
processLine line =
    case lookup (head line) dnsTypeProcessors of
        Nothing -> Nothing
        Just processor -> Just (processor $ tail line)

createSets :: [String] -> [S.Set String]
createSets = createListOfSets [] where
    createListOfSets list [] = list
    createListOfSets list (line:other) = case processLine line of
        Nothing -> createListOfSets list other
        Just hosts -> createListOfSets (S.fromList hosts : list) other

parse :: String -> IO [S.Set String]
parse fileName = do
	fileExists <- doesFileExist fileName
	if fileExists then do
		contents <- readFile fileName
		return $ createSets $ lines contents
	else
		return []
