HUnitTestExc.lhs  --  test for HUnit, using Haskell language system "Exc"

$Id: HUnitTestExc.lhs,v 1.1.2.1 2005/02/07 16:12:13 abayley Exp $

> module Test.HUnit.Main (main) where

> import Test.HUnit
> import Test.HUnit.HUnitTestBase
> import qualified Control.Exception (assert)


 assertionMessage = "HUnitTestExc.lhs:13: Assertion failed\n"
 assertion = Exception.assert False (return ())



> main = runTestTT (test [baseTests, excTests])

> excTests = test [

    -- Hugs and GHC don't currently catch arithmetic exceptions.
   "div by 0" ~:
     expectUnspecifiedError (TestCase ((3 `div` 0) `seq` return ())),

    -- GHC doesn't currently catch array-related exceptions.
   "array ref out of bounds" ~:
     expectUnspecifiedError (TestCase (... `seq` return ())),

>   "error" ~:
>     expectError "error" (TestCase (error "error")),

>   "tail []" ~:
>     expectUnspecifiedError (TestCase (tail [] `seq` return ()))

   -- Hugs doesn't provide `assert`.
   "assert" ~:
     expectError assertionMessage (TestCase assertion)

>  ]
