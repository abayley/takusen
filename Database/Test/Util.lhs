> module Database.Test.Util where

> import Database.Util
> import Test.MiniUnit
> import Data.Int
> import Data.Char


> runTest :: IO ()
> runTest = do
>   counts <- runTestTT "Util module tests" testlist
>   return ()

> testlist = 
>   [ testSubstr
>   , testWordsBy
>   , testInt64ToDatePartsMinDate
>   , testInt64ToDatePartsMaxDate
>   , testUtcTimeToInt64MinDate
>   , testUtcTimeToInt64MaxDate
>   , testCalTimeToInt64MinDate
>   , testCalTimeToInt64MaxDate
>   , testInt64ToCalTimeMinDate
>   , testInt64ToCalTimeMaxDate
>   , testPGDatetimeToUTCTimeMinDate
>   , testPGDatetimeToUTCTimeMaxDate
>   , testPGDatetimeToUTCTimeBCBoundary
>   , testPGDatetimeToUTCTimeBCBoundary2
>   , testUTCTimeToPGDatetimeMinDate
>   , testUTCTimeToPGDatetimeMaxDate
>   ]

> testSubstr = do
>   assertEqual "substr - 1" "a" (substr 1 1 "abc")
>   assertEqual "substr - 2" "b" (substr 2 1 "abc")
>   assertEqual "substr - 3" "c" (substr 3 1 "abc")
>   assertEqual "substr - 4" "bc" (substr 2 2 "abc")
>   assertEqual "substr - 5" "ab" (substr 1 2 "abc")
>   assertEqual "substr - 6" "abc" (substr 1 4 "abc")
>   assertEqual "substr -76" "" (substr 4 4 "abc")

> testWordsBy = do
>   assertEqual "wordsBy - 1" [] (wordsBy isDigit "")
>   assertEqual "wordsBy - 2" [] (wordsBy isDigit " --,")
>   assertEqual "wordsBy - 2.5" ["5"] (wordsBy isDigit "5")
>   assertEqual "wordsBy - 2.75" ["5"] (wordsBy isDigit "5 ")
>   assertEqual "wordsBy - 3" ["5"] (wordsBy isDigit "5 --,")
>   assertEqual "wordsBy - 4" ["6"] (wordsBy isDigit " --,6")
>   assertEqual "wordsBy - 5" ["7"] (wordsBy isDigit " --7, ")
>   assertEqual "wordsBy - 6" ["100", "12", "67", "25"] (wordsBy isDigit "100-12,67--, 25")


-4712 (4713BC?) is a very common minimum date among DBMS implementations.
Maximum varies, but is often 5874897 AD.

> testInt64ToDatePartsMinDate = do
>   let expect :: (Int64,Int64,Int64,Int64,Int64,Int64); expect = (-4712,1,1,0,0,0)
>   assertEqual "testInt64ToDatePartsMinDate" expect (int64ToDateParts (-47120101000000))

> testInt64ToDatePartsMaxDate = do
>   let expect :: (Int64,Int64,Int64,Int64,Int64,Int64); expect = (999999,12,31,23,59,59)
>   assertEqual "testInt64ToDatePartsMaxDate" expect (int64ToDateParts (9999991231235959))

> testUtcTimeToInt64MinDate = do
>   let expect :: Int64; expect = (-47120101000000)
>   assertEqual "testUtcTimeToInt64MinDate" expect (utcTimeToInt64 (mkUTCTime (-4712) 1 1 0 0 0))

> testUtcTimeToInt64MaxDate = do
>   let expect :: Int64; expect = (9999991231235959)
>   assertEqual "testUtcTimeToInt64MaxDate" expect (utcTimeToInt64 (mkUTCTime 999999 12 31 23 59 59))

> testCalTimeToInt64MinDate = do
>   let expect :: Int64; expect = (-47120101000000)
>   assertEqual "testCalTimeToInt64MinDate" expect (calTimeToInt64 (mkCalTime (-4712) 1 1 0 0 0))

> testCalTimeToInt64MaxDate = do
>   let expect :: Int64; expect = (9999991231235959)
>   assertEqual "testCalTimeToInt64MaxDate" expect (calTimeToInt64 (mkCalTime 999999 12 31 23 59 59))

> testInt64ToCalTimeMinDate = do
>   let expect = mkCalTime (-4712) 1 1 0 0 0
>   assertEqual "testCalTimeToInt64MaxDate" expect (int64ToCalTime (-47120101000000))

> testInt64ToCalTimeMaxDate = do
>   let expect = mkCalTime 999999 12 31 23 59 59
>   assertEqual "testCalTimeToInt64MaxDate" expect (int64ToCalTime 9999991231235959)

> testPGDatetimeToUTCTimeMinDate = do
>   let expect = mkUTCTime (-4712) 1 1 0 0 0
>   assertEqual "testCalTimeToInt64MinDate" expect (pgDatetimetoUTCTime "4713-01-01 00:00:00 BC")

> testPGDatetimeToUTCTimeMaxDate = do
>   let expect = mkUTCTime 999999 1 1 0 0 0
>   assertEqual "testCalTimeToInt64MaxDate" expect (pgDatetimetoUTCTime "999999-01-01 00:00:00+00")

> testPGDatetimeToUTCTimeBCBoundary = do
>   let expect = mkUTCTime 0 1 1 0 0 0
>   assertEqual "testCalTimeToInt64MinDate" expect (pgDatetimetoUTCTime "0001-01-01 00:00:00 BC")

> testPGDatetimeToUTCTimeBCBoundary2 = do
>   let expect = mkUTCTime 0 12 31 0 0 0
>   assertEqual "testCalTimeToInt64MinDate" expect (pgDatetimetoUTCTime "0001-12-31 00:00:00 BC")

> testUTCTimeToPGDatetimeMinDate = do
>   let expect = "4713-01-01 00:00:00.000000+00 BC"
>   assertEqual "testCalTimeToInt64MinDate" expect (utcTimeToPGDatetime (mkUTCTime (-4712) 1 1 0 0 0))

> testUTCTimeToPGDatetimeMaxDate = do
>   let expect = "999999-01-01 00:00:00.300001+00 AD"
>   assertEqual "testCalTimeToInt64MinDate" expect (utcTimeToPGDatetime (mkUTCTime 999999 1 1 0 0 0.300001))