
|
Module      :  Database.Enumerator
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 
Simple driver module, mainly for testing.
Import test modules and run test suites.
 
This project is hosted by Haskell-libs:
  <http://sf.net/projects/haskell-libs>
 
Source code for this project is at:
  <http://cvs.sf.net/viewcvs.py/haskell-libs/libs/takusen/>


> module Main where

> import Database.Oracle.Test.OCIFunctions
> import Database.Test.SimpleEnumeratorTest
> import Database.Test.PerformanceTest

> main :: IO ()
> main = do
>   runOCITest
>   runSimpleTest
>   runPerformanceTest
