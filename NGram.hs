module NGram (Host, createHost, findCloseHosts, names) where

import Data.List (sortBy)
import qualified Data.Set as S

data Host = Host {names :: [String], triGrams :: [String], biGrams :: [String]} deriving (Eq)

instance Show Host where
	show host = show $ names host

countSingleTriGrams :: String -> [String]
countSingleTriGrams str = map (\ (a, b, c) -> [a, b, c])
	$ zip3 str (tail str) (tail $ tail str)

countTriGrams :: [String] -> [String]
countTriGrams [] = []
countTriGrams (x:xs) = countSingleTriGrams x ++ countTriGrams xs

countSingleBiGrams :: String -> [String]
countSingleBiGrams str = map (\ (a, b) -> [a, b])
	$ zip str (tail str)

countBiGrams :: [String] -> [String]
countBiGrams [] = []
countBiGrams (x:xs) = countSingleBiGrams x ++ countBiGrams xs

createHost :: [String] -> Host
createHost hosts =
	Host {names = hosts,
		triGrams = countTriGrams hosts,
		biGrams = countBiGrams hosts}

countCommonGrams :: (Host -> [String]) -> Host -> Host -> Double
countCommonGrams
	gramsFunc h1 h2 = fromIntegral $ S.size $ S.intersection
		(S.fromList $ gramsFunc h1)
		(S.fromList $ gramsFunc h2)

countAllGrams :: Host -> Host -> Double
countAllGrams h1 h2 =
	(countCommonGrams triGrams h1 h2) 
		+ (countCommonGrams biGrams h1 h2) * 0.001

sortFunction :: Host -> Host -> Host -> Ordering
sortFunction inputHost h1 h2 =
	compare d2 d1 where -- the more common grams, the less distance, so we like reverse it here
		d1 = countAllGrams inputHost h1
		d2 = countAllGrams inputHost h2

findCloseHosts :: String -> [Host] -> [Host]
findCloseHosts input hosts =
		sortBy (sortFunction $ createHost [input]) hosts
