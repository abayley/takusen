
{-# OPTIONS -fno-monomorphism-restriction #-}

module Test.MiniUnitTest where

import Test.MiniUnit
import Data.IORef
import Control.Exception.MonadIO

main = tests



tests = do
  test__assertFailure
  test__runSingleTestSuccess
  test__runSingleTestFailure
  test__runSingleTestException
  test__reportResults
  test__runTestTT

print_ s = liftIO (putStrLn s)

test__assertFailure = catch
  (assertFailure "test__assertFailure"
    >> error "test__assertFailure: Exception not thrown")
  (\e -> print_ "test__assertFailure OK")

test__runSingleTestSuccess = do
  result <- runSingleTest (return ())
  case result of
    TestSuccess -> print_ "test__runSingleTestSuccess OK"
    TestFailure _ -> print_ "test__runSingleTestSuccess failed"
    TestException _ -> print_ "test__runSingleTest exception"

test__runSingleTestFailure = do
  result <- runSingleTest (assertFailure "test__runSingleTestFailure")
  case result of
    TestSuccess -> print_ "test__runSingleTestFailure failed"
    TestFailure _ -> print_ "test__runSingleTestFailure OK"
    TestException _ -> print_ "test__runSingleTestFailure exception"

test__runSingleTestException = do
  result <- runSingleTest (throwUserError "boo")
  case result of
    TestSuccess -> print_ "test__runSingleTestException failed"
    TestFailure _ -> print_ "test__runSingleTestException failed!"
    TestException _ -> print_ "test__runSingleTestException OK"

test__reportResults = do
  results <- mapM runSingleTest
    [assertFailure "test__runSingleTest", return (), throwUserError "boo"]
  print_ ("report results: " ++ reportResults results)


test__runTestTT = do
  r <- runTestTT "MiniUnitTest" [assertFailure "test__runSingleTest", return (), throwUserError "boo"]
  return ()

----
