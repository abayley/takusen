
|
Module      :  Database.Util
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 
Utility functions. Mostly used in database back-ends, and tests.


> {-# OPTIONS -fglasgow-exts #-}
> {-# OPTIONS -fallow-overlapping-instances #-}
> {-# OPTIONS -fallow-undecidable-instances #-}

> module Database.Util where

> import System.Time
> import Control.Monad.Trans (liftIO)
> import Control.Monad.Reader
> import Data.Int
> import Data.List
> import Data.Char
> import Data.Fixed
> import Data.Time
> import Text.Printf

This little section contains some utility code,
which isn't really specific to our database code.
Perhaps there should be a separate module for this...

MyShow requires overlapping AND undecidable instances.

> class Show a => MyShow a where show_ :: a -> String
> instance MyShow String where show_ s = s
> instance (Show a) => MyShow a where show_ s = show s

| Like 'System.IO.print', except that Strings are not escaped or quoted.

> print_ :: (MonadIO m, MyShow a) => a -> m ()
> print_ s = liftIO (putStrLn (show_ s))

| Convenience for making UTCTimes. Assumes the time given is already UTC time
i.e. there's no timezone adjustment.

> mkUTCTime :: (Integral a, Real b) => a -> a -> a -> a -> a -> b -> UTCTime
> mkUTCTime year month day hour minute second =
>   localTimeToUTC (hoursToTimeZone 0)
>     (LocalTime
>       (fromGregorian (fromIntegral year) (fromIntegral month) (fromIntegral day))
>       (TimeOfDay (fromIntegral hour) (fromIntegral minute) (realToFrac second)))

> mkCalTime :: Integral a => a -> a -> a -> a -> a -> a -> CalendarTime
> mkCalTime year month day hour minute second =
>   CalendarTime
>     { ctYear = fromIntegral year
>     , ctMonth = toEnum (fromIntegral month - 1)
>     , ctDay = fromIntegral day
>     , ctHour = fromIntegral hour
>     , ctMin = fromIntegral minute
>     , ctSec = fromIntegral second
>     , ctPicosec = 0
>     , ctWDay = Sunday
>     , ctYDay = -1
>     , ctTZName = "UTC"
>     , ctTZ = 0
>     , ctIsDST = False
>     }


20040822073512
   10000000000 (10 ^ 10) * year
     100000000 (10 ^ 8) * month
       1000000 (10 ^ 6) * day
         10000  (10^4) * hour

Use quot and rem, /not/ div and mod,
so that we get sensible behaviour for -ve numbers.

> int64ToDateParts :: Int64 -> (Int64, Int64, Int64, Int64, Int64, Int64)
> int64ToDateParts i =
>   let
>     year1 = (i `quot` 10000000000)
>     month = ((abs i) `rem` 10000000000) `quot` 100000000
>     day = ((abs i) `rem` 100000000) `quot` 1000000
>     hour = ((abs i) `rem` 1000000) `quot` 10000
>     minute = ((abs i) `rem` 10000) `quot` 100
>     second = ((abs i) `rem` 100)
>   in (year1, month, day, hour, minute, second)

> datePartsToInt64 ::
>   (Integral a1, Integral a2, Integral a3, Integral a4, Integral a5, Integral a6)
>   => (a1, a2, a3, a4, a5, a6) -> Int64 
> datePartsToInt64 (year, month, day, hour, minute, second) =
>   let
>     yearm :: Int64
>     yearm = 10000000000
>     sign :: Int64
>     sign = if year < 0 then -1 else 1
>   in  yearm * fromIntegral year
>   + sign * 100000000 * fromIntegral month
>   + sign * 1000000 * fromIntegral day
>   + sign * 10000 * fromIntegral hour
>   + sign * 100 * fromIntegral minute
>   + sign * fromIntegral second


> calTimeToInt64 :: CalendarTime -> Int64
> calTimeToInt64 ct =
>   datePartsToInt64
>     ( ctYear ct, fromEnum (ctMonth ct) + 1, ctDay ct
>     , ctHour ct, ctMin ct, ctSec ct)

> utcTimeToInt64 utc =
>   let
>     (LocalTime ltday time) = utcToLocalTime (hoursToTimeZone 0) utc
>     (TimeOfDay hour minute second) = time
>     (year, month, day) = toGregorian ltday
>   in datePartsToInt64 (year, month, day, hour, minute, round second)


> int64ToCalTime :: Int64 -> CalendarTime
> int64ToCalTime i =
>   let (year, month, day, hour, minute, second) = int64ToDateParts i
>   in mkCalTime year month day hour minute second

> int64ToUTCTime :: Int64 -> UTCTime
> int64ToUTCTime i =
>   let (year, month, day, hour, minute, second) = int64ToDateParts i
>   in mkUTCTime year month day hour minute second


> zeroPad n i =
>   if i < 0
>   then "-" ++ (zeroPad n (abs i))
>   else take (n - length (show i)) (repeat '0') ++ show i

> substr i n s = take n (drop (i-1) s)

> wordsBy :: (Char -> Bool) -> String -> [String] 
> wordsBy pred s = skipNonMatch pred s

2 states:
skipNonMatch is for when we are looking for the start of our next word
consumeMatch is for when we are currently scanning a word

> skipNonMatch :: (Char -> Bool) -> String -> [String] 
> skipNonMatch pred "" = []
> skipNonMatch pred (c:cs)
>   | pred c = scanWord pred cs [c]
>   | otherwise = skipNonMatch pred cs

> scanWord pred "" acc = [reverse acc]
> scanWord pred (c:cs) acc
>   | pred c = scanWord pred cs (c:acc)
>   | otherwise = [reverse acc] ++ skipNonMatch pred cs


> positions :: Eq a => [a] -> [a] -> [Int]
> positions [] _ = []
> positions s ins = map fst (filter (isPrefixOf s . snd) (zip [1..] (tails ins)))


 1234567890123456789012345
"2006-11-24 07:51:49.228+00"
"2006-11-24 07:51:49.228"
"2006-11-24 07:51:49.228 BC"
"2006-11-24 07:51:49+00 BC"

FIXME  use TZ to specify timezone?
Not necessary, PostgreSQL always seems to output
+00 for timezone. It's already adjusted the time,
I think. Need to test this with different server timezones, though.

> pgDatetimetoUTCTime :: String -> UTCTime
> pgDatetimetoUTCTime s =
>   let (year, month, day, hour, minute, second, tz) = pgDatetimetoParts s
>   in mkUTCTime year month day hour minute second

> pgDatetimetoCalTime :: String -> CalendarTime
> pgDatetimetoCalTime s =
>   let (year, month, day, hour, minute, second, tz) = pgDatetimetoParts s
>   in mkCalTime year month day hour minute (round second)

> pgDatetimetoParts :: String -> (Int, Int, Int, Int, Int, Double, Int)
> pgDatetimetoParts s =
>   let
>     pred c = isAlphaNum c || c == '.'
>     ws = wordsBy pred s
>     parts :: [Int]; parts = map read (take 5 ws)
>     secs :: Double; secs = read (ws!!5)
>     hasTZ = isInfixOf "+" s
>     tz :: Int; tz = if hasTZ then read (ws !! 6) else 0
>     isBC = isInfixOf "BC" s
>     year :: Int; year = if isBC then (- ((parts !! 0) - 1)) else parts !! 0
>   in (year, (parts !! 1), (parts !! 2)
>      , (parts !! 3), (parts !! 4), secs, tz)

> utcTimeToPGDatetime :: UTCTime -> String
> utcTimeToPGDatetime utc =
>   let
>     (LocalTime ltday time) = utcToLocalTime (hoursToTimeZone 0) utc
>     (TimeOfDay hour minute second) = time
>     (year1, month, day) = toGregorian ltday
>     suffix = if year1 < 1 then " BC" else " AD"
>     year = if year1 < 1 then abs(year1 - 1) else year1
>     s1 :: Double; s1 = realToFrac second
>     secs :: String; secs = printf "%09.6f" s1
>   in zeroPad 4 year
>     ++ "-" ++ zeroPad 2 month
>     ++ "-" ++ zeroPad 2 day
>     ++ " " ++ zeroPad 2 hour
>     ++ ":" ++ zeroPad 2 minute
>     ++ ":" ++ secs
>     ++ "+00" ++ suffix

| Assumes CalendarTime is also UTC i.e. ignores ctTZ component.

> calTimeToPGDatetime :: CalendarTime -> String
> calTimeToPGDatetime ct =
>   let
>     (year1, month, day, hour, minute, second, pico, tzsecs) =
>       ( ctYear ct, fromEnum (ctMonth ct) + 1, ctDay ct
>       , ctHour ct, ctMin ct, ctSec ct, ctPicosec ct, ctTZ ct)
>     suffix = if year1 < 1 then " BC" else " AD"
>     year = if year1 < 1 then abs(year1 - 1) else year1
>     s1 :: Double; s1 = realToFrac second + ((fromIntegral pico) / (10.0 ^ 12) )
>     secs :: String; secs = printf "%09.6f" s1
>   in zeroPad 4 year
>     ++ "-" ++ zeroPad 2 month
>     ++ "-" ++ zeroPad 2 day
>     ++ " " ++ zeroPad 2 hour
>     ++ ":" ++ zeroPad 2 minute
>     ++ ":" ++ secs
>     ++ "+00" ++ suffix
