
module Test.MiniUnit where

import Control.Exception.MonadIO
import Control.Exception
import Control.Monad
import System.IO.Error (ioeGetErrorString)
import Data.List
import Data.IORef


data TestResult = TestSuccess | TestFailure String | TestException String
  deriving (Show, Eq)

{-
We'll use HUnit's trick of throwing an IOError when an assertion fails.
This will terminate the test case, obviously, but we catch the exception
and record that it haa failed so that we can continue with other
test cases.

Unlike HUnit, we catch all exceptions; any that are not thrown by
failed assertions are recorded as test errors (as opposed to test failures),
and the testing continues...
-}

throwUserError :: CaughtMonadIO m => String -> m ()
throwUserError msg = liftIO (throwIO (IOException (userError msg)))

-- When an assertion fails, we throw an IOException with a special
-- prefix, which the exception handler will detect.
assertFailure :: CaughtMonadIO m => String -> m ()
assertFailure msg = throwUserError (exceptionPrefix ++ msg)

exceptionPrefix = "MiniUnit:"
hugsPrefix  = "IO Error: User error\nReason: "
nhc98Prefix = "I/O error (user-defined), call to function `userError':\n  "
ghcPrefix = ""  -- We don't use this; it's just documentation...

dropPrefix p s = if isPrefixOf p s then drop (length p) s else s
trimCompilerPrefixes = dropPrefix hugsPrefix . dropPrefix nhc98Prefix

runSingleTest :: CaughtMonadIO m => m () -> m TestResult
runSingleTest action = do
  result <- gtry action
  case result of
    Right _ -> return TestSuccess
    Left e -> do
      case ioErrors e of
        Nothing -> return (TestException (show e))
        Just ioe -> do
          let errText = trimCompilerPrefixes (ioeGetErrorString ioe)
          if isPrefixOf exceptionPrefix errText
            then return (TestFailure (dropPrefix exceptionPrefix errText))
            else return (TestException (show e))

-- Predicates for list filtering
isSuccess TestSuccess = True
isSuccess _ = False
isFailure (TestFailure _) = True
isFailure _ = False
isError (TestException _) = True
isError _ = False

-- Make function composition look more like Unix pipes.
-- This first definition requires a Point-Free Style.
-- I prefer the PFS, as you can use it in (for example) predicate
-- functions passed as arguments (see filter example below).
infixl 9 |>
(|>) = flip (.)

-- This second definition affords a more pointed style...
-- We can use this operator to inject an argument into a pipe
-- defined using |>; it has lower precedence, so will bind last.
-- e.g. ... = mylist |>> zip [1..] |> filter (snd |> pred) |> map show |> concat
infixl 8 |>>
(|>>) = flip ($)

reportFilter pred = zip [1..] |> filter (snd |> pred) |> map testReporter |> concat

testReporter (n, TestSuccess) = ""
testReporter (n, TestException s) = "Test " ++ show n ++ " failed with exception:\n" ++ s ++ "\n"
testReporter (n, TestFailure s) = "Test " ++ show n ++ " failed with message:\n" ++ s ++ "\n"

reportResults list =
  let
    s = list |>> filter isSuccess |> length
    e = list |>> filter isError   |> length
    f = list |>> filter isFailure |> length
  in "Test cases: " ++ show (length list)
  ++ "  Failures: " ++ show f
  ++ "  Errors: " ++ show e
  ++ "\n"
  -- ++ reportFilter isFailure list
  -- ++ reportFilter isError list

-- 2 defns for same result; which is better?
--contains pred = filter pred |> null |> not
contains p l = maybe False (const True) (find p l)

-- | Return 0 if everything is rosy,
-- 1 if there were test failures (but no exceptions),
-- 2 if there were any exceptions.
-- You could use this return code as the return code from
-- your program, if you're driving from the command line.
runTestTT :: CaughtMonadIO m => [m ()] -> m Int
runTestTT list = do
  liftIO (putStrLn "")
  liftIO (putStrLn ("Test case count: " ++ show (length list)))
  --r <- mapM (\t -> liftIO (putStr ".") >> runSingleTest n t) list
  r <- mapM (\(n, t) -> liftIO (putStr ".") >> runSingleTestTT n t) (zip [1..] list)
  liftIO (putStrLn "")
  liftIO (putStrLn (reportResults r))
  if contains isError r
    then return 2
    else if contains isFailure r
      then return 1
      else return 0

-- Could use this instead of runSingleTest - it will output
-- failures and exceptions as they occur, rather than all
-- at the end.
runSingleTestTT :: CaughtMonadIO m => Int -> m () -> m TestResult
runSingleTestTT n test = do
  r <- runSingleTest test
  case r of
    TestSuccess -> return r
    TestFailure _ -> liftIO (putStrLn ('\n':(testReporter (n ,r)))) >> return r
    TestException _ -> liftIO (putStrLn ('\n':(testReporter (n, r)))) >> return r

---------------------------------------------
-- That's the basic framework; now for some sugar...
-- ... stolen straight from Dean's HUnit code.
-- Shall we steal his infix operators, too?

assertBool :: CaughtMonadIO m => String -> Bool -> m ()
assertBool msg b = unless b (assertFailure msg)

assertString :: CaughtMonadIO m => String -> m ()
assertString s = unless (null s) (assertFailure s)

assertEqual :: (Eq a, Show a, CaughtMonadIO m) => String -> a -> a -> m ()
assertEqual preface expected actual = do
  let
    msg =
      (if null preface then "" else preface ++ "\n")
      ++ "expect: " ++ show expected ++ "\nactual: " ++ show actual
  unless (actual == expected) (assertFailure msg)


--p @? msg = assertBool msg p
