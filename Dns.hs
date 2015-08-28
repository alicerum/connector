module Dns
(
	parse
) where

import System.Directory
import qualified Data.Set as S

dnsTypeProcessors = 
	[('C', processAlias)]

processAlias :: String -> String
processAlias = takeWhile (\ x -> x /= ':')

processLine :: String -> Maybe String
processLine [] = Nothing
processLine line = 
	case (lookup (head line) dnsTypeProcessors) of
		Nothing -> Nothing
		Just processor -> Just (processor $ tail line)

createSet :: [String] -> S.Set String
createSet lines =
	toSet lines S.empty where
		toSet [] set = set
		toSet (line:other) set = 
			case processLine line of
				Nothing -> toSet other set
				Just host -> toSet other $ S.insert host set

buildPairs :: [String] -> [String]
buildPairs = S.toList . createSet

parse :: String -> IO [String]
parse fileName = do
	fileExists <- doesFileExist fileName
	if fileExists then do
		contents <- readFile fileName
		return $ buildPairs $ lines contents
	else
		return []
