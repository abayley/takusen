
|
Module      :  Database.Enumerator
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 
Simple driver module, mainly for testing.
Import test modules and run test suites.


> module Main where

> import Database.Test.PerformanceTest
> import Database.Test.SimpleEnumeratorTest
> import Database.Oracle.Test.OCIFunctions

> main :: IO ()
> main = do
>   runOCITest
>   runSimpleTest
>   runPerformanceTest
