> module Database.Test.Util where

> import Database.Util
> import Test.MiniUnit
> import Data.Int
> import Data.Char
> import Foreign.C.UTF8
> import Foreign.C.String
> import Foreign.Marshal.Array
> import Foreign.Ptr
> import Word (Word8)
> import qualified Test.QuickCheck as QC


> runTest :: IO ()
> runTest = do
>   counts <- runTestTT "Util module tests" testlist
>   return ()

> testlist = 
>   [ testSubstr
>   , testWordsBy
>   , testUTF8RoundTrip
>   , quickCheckUTF8RoundTrip
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

> instance QC.Arbitrary Char where
>   arbitrary = QC.choose (chr 1, chr 0x10FFFF)
>   coarbitrary = undefined

> prop_roundtrip s = s == fromUTF8 (toUTF8 s)
> quickCheckUTF8RoundTrip = QC.test prop_roundtrip

> testUTF8RoundTrip = do
>   utf8RoundTrip "1ByteLow"   0x000001 [0x01]
>   utf8RoundTrip "1ByteHigh"  0x00007F [0x7F]
>   utf8RoundTrip "2BytesLow"  0x000080 [0xC2, 0x80]
>   utf8RoundTrip "2BytesHigh" 0x0007FF [0xDF, 0xBF]
>   utf8RoundTrip "3BytesLow"  0x000800 [0xE0, 0xA0, 0x80]
>   utf8RoundTrip "3BytesHigh" 0x00FFFF [0xEF, 0xBF, 0xBF]
>   utf8RoundTrip "4BytesLow"  0x010000 [0xF0, 0x90, 0x80, 0x80]
>   -- chr 0x10FFFF is the largest code-point Haskell currently allows
>   utf8RoundTrip "4BytesHigh" 0x10FFFF [0xF4, 0x8F, 0xBF, 0xBF]

> utf8RoundTrip :: String -> Int -> [Word8] -> IO ()
> utf8RoundTrip msg unicode utf8 = do
>   assertEqual ("testToUTF8-" ++ msg) utf8 (toUTF8 [chr unicode])
>   assertEqual ("testFromUTF8-" ++ msg) [chr unicode] (fromUTF8 utf8)
>   withUTF8String [chr unicode] $ \cstr -> do
>     w8 <- peekArray0 0 (castPtr cstr)
>     assertEqual ("testToUTF8Ptr-" ++ msg) utf8 w8
>   withCString (map (chr . fromIntegral) utf8) $ \cstr -> do
>     s <- peekUTF8String cstr
>     assertEqual ("testFromUTF8Ptr-" ++ msg) [chr unicode] s


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
