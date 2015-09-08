module NGram (Host, createHost, findCloseHosts, names) where

import Data.List (sortBy)
import qualified Data.Set as S

data Host = Host {names :: [String], triGrams :: S.Set String, biGrams :: S.Set String} deriving (Eq)

instance Show Host where
    show host = showHosts $ names host

showHosts :: [String] -> String
showHosts [] = ""
showHosts (host:other) = host ++ showOther (take 2 other) where
    showOther [] = []
    showOther hosts = "\t\t\t(" ++ unwords hosts ++ ")"

countSingleTriGrams :: String -> [String]
countSingleTriGrams str = map (\ (a, b, c) -> [a, b, c])
        $ zip3 str (tail str) (tail $ tail str)

countTriGrams :: [String] -> [String]
countTriGrams = foldr ((++) . countSingleTriGrams) []

countSingleBiGrams :: String -> [String]
countSingleBiGrams str = map (\ (a, b) -> [a, b])
        $ zip str (tail str)

countBiGrams :: [String] -> [String]
countBiGrams = foldr ((++) . countSingleBiGrams) []


sortHosts :: String -> String -> Ordering
sortHosts h1 h2 = compare (length h2) (length h1)

createHost :: [String] -> Host
createHost hosts =
        Host {names = sortBy sortHosts hosts,
                triGrams = S.fromList $ countTriGrams hosts,
                biGrams = S.fromList $ countBiGrams hosts}

countCommonGrams :: (Host -> S.Set String) -> Host -> Host -> Double
countCommonGrams
        gramsFunc h1 h2 = fromIntegral $ S.size $ S.intersection
                (gramsFunc h1)
                (gramsFunc h2)

countAllGrams :: Host -> Host -> Double
countAllGrams h1 h2 =
        countCommonGrams triGrams h1 h2
                + countCommonGrams biGrams h1 h2 * 0.001

sortFunction :: Host -> Host -> Host -> Ordering
sortFunction inputHost h1 h2 =
        compare d2 d1 where -- the more common grams, the less distance, so we like reverse it here
                d1 = countAllGrams inputHost h1
                d2 = countAllGrams inputHost h2

findCloseHosts :: String -> [Host] -> [Host]
findCloseHosts input =
        sortBy (sortFunction inputHost)
            where inputHost = createHost [input]

