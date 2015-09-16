import System.Environment
import UI.NCurses
import qualified Dns as D
import qualified NGram as N
import Data.Char(ord)
import qualified Data.Set as S
import System.Process
import Control.Monad (when, unless)
import Data.Foldable as Foldable (forM_)

inviteString = "Enter ur stuff: "
allowedInput = ['a'..'z'] ++ ['0'..'9'] ++ "-."

showResult :: Window -> Integer -> String -> [N.Host] -> Curses (Maybe N.Host)
showResult w selection input hosts = do
    (rows, cols) <- screenSize
    let closeHosts = N.findCloseHosts input hosts
    updateWindow w $ do
        when (length input > 2) $ do
            clearHosts w rows cols
            drawHosts selection (take (fromIntegral (rows-1)) closeHosts) 1
        moveCursor 0 0
        drawString $ replicate (fromIntegral (rows-1)) ' '
        moveCursor 0 0
        drawString $ inviteString ++ input
    render
    getInput w selection input closeHosts

getHost :: Integer -> [N.Host] -> N.Host
getHost sel hosts =
    if sel == 0
    then head hosts
    else hosts !! fromIntegral (sel - 1)

reactOnCharacter :: Window -> Integer -> Char -> String -> [N.Host] -> Curses (Maybe N.Host)
reactOnCharacter w selection c input hosts =
    if c `elem` allowedInput
    then showResult w 0 (input ++ [c]) hosts
    else case ord c of
        10 -> return $ Just $ getHost selection hosts
        27 -> return Nothing
        _  -> getInput w 0 input hosts

delChar :: String -> String
delChar [] = []
delChar x = init x

decSel :: Integer -> Integer -> Integer
decSel rows cur =
    if cur-1 <= 0
    then 1
    else cur - 1

incSel :: Integer -> Integer -> Integer
incSel rows cur =
    if cur+1 == rows
    then cur
    else cur+1

reactOnKey :: Window -> Integer -> Key -> String -> [N.Host] -> Curses (Maybe N.Host)
reactOnKey w selection k input hosts = do
    (rows, _) <- screenSize
    case k of KeyBackspace -> showResult w 0 (delChar input) hosts
              KeyUpArrow -> showResult w (decSel rows selection) input hosts
              KeyDownArrow -> showResult w (incSel rows selection) input hosts
              _ -> getInput w selection input hosts

reactOnEvent ::	Window -> Integer -> Event -> String -> [N.Host] -> Curses (Maybe N.Host)
reactOnEvent w selection ev input hosts =
    case ev of EventCharacter c -> reactOnCharacter w selection c input hosts
               EventSpecialKey key -> reactOnKey w selection key input hosts

getInput :: Window -> Integer -> String -> [N.Host] -> Curses (Maybe N.Host)
getInput w selection curInput hosts = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' -> reactOnEvent w selection ev' curInput hosts

clearHostLines :: Integer -> Integer -> Integer -> Update ()
clearHostLines curRow rows columns =
    unless ((curRow+1) == rows) $ do
        moveCursor curRow 0
        drawString $ replicate (fromInteger columns) ' '
        clearHostLines (curRow+1) rows columns

clearHosts :: Window -> Integer -> Integer -> Update ()
clearHosts w = clearHostLines 1

drawHosts :: Integer ->[N.Host] -> Integer -> Update ()
drawHosts _ [] _ = return ()
drawHosts selection (host:other) line = do
    moveCursor line 3
    drawString $ show host
    when (selection == line) $ do
        moveCursor line 0
        drawString "*"
    drawHosts selection other (line+1)

goIntoCurses :: [N.Host] -> IO (Maybe N.Host)
goIntoCurses hosts = runCurses $ do
    setEcho False
    w <- defaultWindow
    updateWindow w $ do
        moveCursor 0 0
        drawString inviteString
    render
    getInput w 0 "" hosts

runSsh :: String -> N.Host -> IO ()
runSsh login host = do
    code <- system $ "ssh " ++ login ++ "@" ++ head (N.names host)
    return ()
	
main :: IO ()
main = do
    args <- getArgs
    if length args /= 2
    then putStrLn "Usage: connector dns login"
    else do
        let fileName = head args
            login = head $ tail args
        lines <- D.parse fileName
        if null lines
        then putStrLn "Dns file empty or does not exist"
        else do
            host <- goIntoCurses $ map (N.createHost . S.toList) lines
            Foldable.forM_ host (runSsh login)
