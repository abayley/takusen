
module Bird2Hs where
import System.Environment (getArgs)
import System.IO (hPutStrLn, hIsEOF, openFile, hClose, hGetLine, IOMode(..))
import Data.List (isPrefixOf, tails)
import Data.Char (isSpace)


hasDotLhsSuffix = isPrefixOf "shl." . reverse

removeDotLhsSuffix = reverse . (drop 4) . reverse

replaceSuffix f = (removeDotLhsSuffix f) ++ ".hs"

outFileName f = 
  if hasDotLhsSuffix f
  then replaceSuffix f
  else f ++ ".hs"

inFileName f =
  if hasDotLhsSuffix f
  then f
  else f ++ ".lhs"

trimLeading = dropWhile isSpace
trimTrailing = reverse . dropWhile isSpace . reverse
contains s = all (isPrefixOf s) . tails

isBirdTrack = isPrefixOf ">"

isLineComment = isPrefixOf "--"
isCodeStart = isPrefixOf "\\begin{code}"
isCodeEnd = isPrefixOf "\\end{code}"


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
      | null line = transition line stateComment
      | isLineComment line = transition line stateComment
      | isBirdTrack line = transition (drop 2 line) stateBirdTrack
      | isCodeStart line = transition ("-- " ++ line) stateCodeSection
      | otherwise = transition ("-- " ++ line) stateComment

    stateBirdTrack line
      | isBirdTrack line = transition (drop 2 line) stateBirdTrack
      | otherwise = transition line stateComment

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
