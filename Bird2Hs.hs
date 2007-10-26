
module Bird2Hs where

import System.Environment (getArgs)
import System.IO (hPutStrLn, hIsEOF, openFile, hClose, hGetLine, IOMode(..))
import Data.List (isPrefixOf)

isBirdTrack = isPrefixOf ">"
-- isCpp test might not be quite correct, but it'll do for Takusen for now.
isCpp = isPrefixOf "#"
isLineComment = isPrefixOf "--"
isCodeStart = isPrefixOf "\\begin{code}"
isCodeEnd = isPrefixOf "\\end{code}"
isEmptyLine s = null s || s == "\r"


startStateMachine handleIn handleOut = stateComment ""
  where
    transition output nextState = do
      hPutStrLn handleOut output
      eof <- hIsEOF handleIn
      if eof
        then return ()
        else do
          nextLine <- hGetLine handleIn
          nextState nextLine

    stateComment line
      | isEmptyLine line || isLineComment line || isCpp line
          = transition line stateComment
      | isBirdTrack line = transition (drop 2 line) stateBirdTrack
      | isCodeStart line = transition ("-- " ++ line) stateCodeSection
      | otherwise = transition ("-- " ++ line) stateComment

    stateBirdTrack line
      | isCpp line || isEmptyLine line = transition line stateBirdTrack
      | isBirdTrack line = transition (drop 2 line) stateBirdTrack
      | otherwise = transition ("-- " ++ line) stateComment

    stateCodeSection line
      | isCodeStart line = error "encountered \\begin{code} in code!?"
      | isCodeEnd line = transition ("-- " ++ line) stateComment
      | otherwise = transition line stateCodeSection


bird2hs fileNameIn fileNameOut = do
  --putStrLn ("bird2hs: " ++ fileNameIn ++ " -> " ++ fileNameOut)
  handleIn <- openFile fileNameIn ReadMode
  handleOut <- openFile fileNameOut WriteMode
  startStateMachine handleIn handleOut
  hClose handleIn
  hClose handleOut
