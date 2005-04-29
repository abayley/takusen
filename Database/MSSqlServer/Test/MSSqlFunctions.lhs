> {-# OPTIONS -fglasgow-exts #-}

|
Module      :  Database.MSSqlServer.Test.MSSqlFunctions
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 


> module Database.MSSqlServer.Test.MSSqlFunctions (runTest) where


> import Foreign
> import Foreign.C
> import Control.Monad
> import Control.Exception
> import Data.Dynamic
> import Database.MSSqlServer.MSSqlFunctions
> import System.Environment (getArgs)
> import Test.HUnit



> runTest :: IO ()
> runTest = printIgnoreError$ do
>     [ usr, pwd, svr ] <- getArgs
>     putStrLn "init"
>     dbInit
>     putStrLn "install error handler"
>     enableErrorHandler
>     putStrLn "open"
>     sess <- open usr pwd "takusen" svr
>     putStrLn "close"
>     dbClose sess
>     putStrLn "exit"
>     dbExit
>     return ()


> testlist db = TestList $ map (\t -> TestCase (t db))
>   [ ]


> ignoreError action =
>   catchMSSql action (\e -> return undefined)

> printIgnoreError action = catchMSSql action 
>     (\e -> do
>       putStrLn (show e)
>       return undefined
>     )

> printPropagateError action = catchMSSql action 
>     (\e -> do
>       putStrLn (show e)
>       return undefined
>     )
