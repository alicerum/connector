module Dns (parse) where

import System.Directory
import qualified Data.Map as M
import qualified Data.Set as S

type Ip = String
type Hostname = String
type HostMap = M.Map Ip (S.Set Hostname)

emptyMap :: HostMap
emptyMap = M.fromList []

hostListToMap :: [(Ip, S.Set Hostname)] -> HostMap
hostListToMap [] = emptyMap
hostListToMap list = M.fromList list

addIp :: HostMap -> Ip -> Hostname -> HostMap
addIp hosts ip host = case M.lookup ip hosts of
    Nothing -> M.insert ip (S.fromList [host]) hosts
    Just set -> M.insert ip (S.insert host set) hosts

addAlias :: HostMap -> Hostname -> Hostname -> HostMap
addAlias hosts h1 h2 = hostListToMap $ findInsert (M.toList hosts) h1 h2 where
    findInsert [] h1 h2 = []
    findInsert (record:others) h1 h2 = appendHost record h1 h2 : findInsert others h1 h2 where
        appendHost (ip, set) h1 h2 = if S.member h2 set
                                     then (ip, S.insert h1 set)
                                     else (ip, set)

getPart :: String -> String
getPart = takeWhile (/= ':')

dropPart :: String -> String
dropPart = dropWhile (/= ':')

processIp :: HostMap -> String -> HostMap
processIp hosts line = addIp hosts ip host where
    host = getPart line
    ip = (getPart . tail . dropPart) line

processAlias :: HostMap -> String -> HostMap
processAlias hosts line = addAlias hosts h1 h2 where
    h1 = getPart line
    h2 = (getPart . tail . dropPart) line

dnsProcessors :: [(Char, HostMap -> String -> HostMap)]
dnsProcessors = [('C', processAlias),
                 ('+', processIp),
                 ('=', processIp)]

processLine :: HostMap -> String -> HostMap
processLine hosts [] = hosts
processLine hosts line = case lookup (head line) dnsProcessors of
    Nothing -> hosts
    Just processor -> processor hosts (tail line)

process :: [String] -> HostMap
process = foldl processLine emptyMap

parse :: String -> IO [S.Set String]
parse fileName = do
    fileExists <- doesFileExist fileName
    if fileExists then do
        contents <- readFile fileName
        return $ M.elems $ process $ lines contents
    else
        return []
