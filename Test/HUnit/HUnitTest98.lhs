HUnitTest98.lhs  --  test for HUnit, using Haskell language system "98"

$Id: HUnitTest98.lhs,v 1.1.2.1 2005/02/07 16:12:13 abayley Exp $

> module Test.HUnit.Main (main) where

> import Test.HUnit
> import Test.HUnit.HUnitTestBase


> main = runTestTT (test [baseTests])
