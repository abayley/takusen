
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

> import Data.Time
> import System.Time
> import Control.Monad.Trans (liftIO)
> import Control.Monad.Reader
> import Data.Int

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

> mkUTCTime :: Integral a => a -> a -> a -> a -> a -> a -> UTCTime
> mkUTCTime year month day hour minute second =
>   localTimeToUTC (hoursToTimeZone 0)
>     (LocalTime
>       (fromGregorian (fromIntegral year) (fromIntegral month) (fromIntegral day))
>       (TimeOfDay (fromIntegral hour) (fromIntegral minute) (fromIntegral second)))



20040822073512
   10000000000 (10 ^ 10) * year
     100000000 (10 ^ 8) * month
       1000000 (10 ^ 6) * day
         10000  (10^4) * hour

Use quot and rem, /not/ div and mod,
so that we get sensible behaviour for -ve numbers.

> int64ToCalTime :: Int64 -> CalendarTime
> int64ToCalTime i =
>   let
>     year = (i `quot` 10000000000)
>     month = ((abs i) `rem` 10000000000) `quot` 100000000
>     day = ((abs i) `rem` 100000000) `quot` 1000000
>     hour = ((abs i) `rem` 1000000) `quot` 10000
>     minute = ((abs i) `rem` 10000) `quot` 100
>     second = ((abs i) `rem` 100)
>   in CalendarTime
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

> calTimeToInt64 :: CalendarTime -> Int64
> calTimeToInt64 ct =
>   let
>     yearm :: Int64
>     yearm = 10000000000
>   in  yearm * fromIntegral (ctYear ct)
>   + 100000000 * fromIntegral ((fromEnum (ctMonth ct) + 1))
>   + 1000000 * fromIntegral (ctDay ct)
>   + 10000 * fromIntegral (ctHour ct)
>   + 100 * fromIntegral (ctMin ct)
>   + fromIntegral (ctSec ct)


> int64ToUTCTime :: Int64 -> UTCTime
> int64ToUTCTime i =
>   let
>     year = (i `quot` 10000000000)
>     month = ((abs i) `rem` 10000000000) `quot` 100000000
>     day = ((abs i) `rem` 100000000) `quot` 1000000
>     hour = ((abs i) `rem` 1000000) `quot` 10000
>     minute = ((abs i) `rem` 10000) `quot` 100
>     second = ((abs i) `rem` 100)
>   in mkUTCTime
>     (fromIntegral year) (fromIntegral month) (fromIntegral day)
>     (fromIntegral hour) (fromIntegral minute) (fromIntegral second)


> utcTimeToInt64 utc =
>   let
>     (LocalTime ltday time) = utcToLocalTime (hoursToTimeZone 0) utc
>     (TimeOfDay hour minute second) = time
>     (year, month, day) = toGregorian ltday
>     yearm :: Int64
>     yearm = 10000000000
>   in  yearm * fromIntegral year
>   + 100000000 * fromIntegral month
>   + 1000000 * fromIntegral day
>   + 10000 * fromIntegral hour
>   + 100 * fromIntegral minute
>   + fromIntegral (round second)
