
-- |
-- Module      :  Foreign.C.UTF8
-- Copyright   :  (c) 2004 John Meacham, Alistair Bayley
-- License     :  BSD-style
-- Maintainer  :  alistair@abayley.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Marshall Haskell Strings to and from UTF8-encoded CStrings.
-- This module's code is inspired by John Meacham's UTF8 en- & de-coders,
-- and also those found in the HXT library (module Text.XML.HXT.DOM.Unicode).

module Foreign.C.UTF8
  ( peekUTF8String, newUTF8String, withUTF8String, withUTF8StringLen
  , toUTF8String, fromUTF8String
  , lengthUTF8, fromUTF8, toUTF8
  ) where

import Control.Monad (when)
import Data.Bits
import Data.Char
import Data.Word (Word8)
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable

nullCChar :: CChar
nullCChar = 0

nullByte :: Word8
nullByte = 0

-- | Analogous to peekCString. Converts UTF8 CString to String.
peekUTF8String :: CString -> IO String
peekUTF8String cs = fromUTF8Ptr (castPtr cs)

-- | Analogous to newCString. Creates UTF8 encoded CString.
newUTF8String :: String -> IO CString
newUTF8String hs = do
  p <- newArray0 nullByte (toUTF8 hs)
  return (castPtr p)

-- | Analogous to withCString. Creates UTF8 encoded CString.
withUTF8String :: String -> (CString -> IO a) -> IO a
withUTF8String s action =
  withUTF8StringLen s $ \(cstr, _) -> action cstr

-- | Analogous to withCStringLen.
withUTF8StringLen :: String -> (CStringLen -> IO a) -> IO a
withUTF8StringLen s action = do
  let utf8Str = toUTF8 s
  let lenUtf8 = length utf8Str
  allocaArray0 lenUtf8 $ \ptr -> do
    pokeArray0 nullByte ptr utf8Str
    action (castPtr ptr, lenUtf8)

-- | Convert a String that was marshalled from a CString without
-- any decoder applied. This might be useful if the client encoding
-- is unknown, and the user code must convert.
-- We assume that the UTF8 CString was marshalled as if Latin-1
-- i.e. all chars are in the range 0-255.
fromUTF8String :: String -> String
fromUTF8String = fromUTF8 . map charToWord8

-- | Convert a Haskell String into a UTF8 String, where each UTF8 byte
-- is represented by its Char equivalent i.e. only chars 0-255 are used.
-- The resulting String can be marshalled to CString directly i.e. with
-- a Latin-1 encoding.
toUTF8String :: String -> String
toUTF8String = map word8ToChar . toUTF8

charToWord8 :: Char -> Word8
charToWord8 = fromIntegral . fromEnum

word8ToChar :: Word8 -> Char
word8ToChar = toEnum . fromIntegral

lengthUTF8 :: String -> Int
lengthUTF8 s = length (toUTF8 s)

{-
The codepoint-to-UTF8 rules:
0x00 - 0x7f: 7 bits: as is
0x80 - 0x07ff: 11 bits
  byte 1: 0xC0 OR ((x <<  6) AND 0x1F)  i.e. 0xC0 + bits  7-11 (bits 12-up are 0)
  byte 2: 0x80 OR (x         AND 0x3F)  i.e. 0x80 + bits  1-6
0x0800 - 0xFFFF: 16 bits
  byte 1: 0xE0 OR ((x << 12) AND 0x0F)  i.e. 0xE0 + bits 13-16
  byte 2: 0x80 OR ((x <<  6) AND 0x3F)  i.e. 0x80 + bits  7-12
  byte 3: 0x80 OR (x         AND 0x3F)  i.e. 0x80 + bits  1-6
0x00010000 - 0x001FFFFF: 21 bits
  byte 1: 0xF0 OR ((x << 18) AND 0x07)  i.e. 0xF0 + bits 19-21
  byte 2: 0x80 OR ((x << 12) AND 0x3F)  i.e. 0x80 + bits 13-18
  byte 3: 0x80 OR ((x <<  6) AND 0x3F)  i.e. 0x80 + bits  7-12
  byte 4: 0x80 OR (x         AND 0x3F)  i.e. 0x80 + bits  1-6
0x00200000 - 0x03FFFFFF: 26 bits
  byte 1: 0xF8 OR ((x << 24) AND 0x03)  i.e. 0xF8 + bits 25-26
  byte 2: 0x80 OR ((x << 18) AND 0x3F)  i.e. 0x80 + bits 19-24
  byte 3: 0x80 OR ((x << 12) AND 0x3F)  i.e. 0x80 + bits 13-18
  byte 4: 0x80 OR ((x <<  6) AND 0x3F)  i.e. 0x80 + bits  7-12
  byte 5: 0x80 OR (x         AND 0x3F)  i.e. 0x80 + bits  1-6
0x04000000 - 0x7FFFFFFF: 31 bits
  byte 1: 0xFC OR ((x << 30) AND 0x01)  i.e. 0xFC + bit  31
  byte 2: 0x80 OR ((x << 24) AND 0x3F)  i.e. 0x80 + bits 25-30
  byte 3: 0x80 OR ((x << 18) AND 0x3F)  i.e. 0x80 + bits 19-24
  byte 4: 0x80 OR ((x << 12) AND 0x3F)  i.e. 0x80 + bits 13-18
  byte 5: 0x80 OR ((x <<  6) AND 0x3F)  i.e. 0x80 + bits  7-12
  byte 6: 0x80 OR (x         AND 0x3F)  i.e. 0x80 + bits  1-6
-}

-- | Convert Unicode characters to UTF-8.
toUTF8 :: String -> [Word8]
toUTF8 [] = []
toUTF8 (x:xs) = toUTF8' (ord x) where
  toUTF8' x
      | x <= 0x0000007F = fromIntegral x : more
      | x <= 0x000007FF
        = w8 0xC0  6 : w8 0x80  0 : more
      | x <= 0x0000FFFF
        = w8 0xE0 12 : w8 0x80  6 : w8 0x80  0 : more
      | x <= 0x0010FFFF   -- should be 0x001FFFFF
        = w8 0xF0 18 : w8 0x80 12 : w8 0x80  6 : w8 0x80  0 : more
      | otherwise = error ("toUTF8: codepoint " ++ show x
         ++ " is greater than the largest allowed (decimal 1114111, hex 0x10FFFF).")
      -- Potentially useful code, if Haskell ever supports codepoints > 0x0010FFFF.
      -- There are no tests for this, because we can't create Strings containing
      -- chars > 0x0010FFFF.
      | x <= 0x03FFFFFF
        = w8 0xF8 24 : w8 0x80 18 : w8 0x80 12 : w8 0x80  6 : w8 0x80  0 : more
      | x <= 0x7FFFFFFF
        = w8 0xFC 30 : w8 0x80 24 : w8 0x80 18 : w8 0x80 12 : w8 0x80  6 : w8 0x80  0 : more
      | otherwise = error ("toUTF8: codepoint " ++ show x ++ " is greater "
         ++ "then the largest that can be represented by UTF8 encoding"
         ++ "(decimal 2147483647, hex 0x7FFFFFFF).")
    where
    more = toUTF8 xs
    w8 :: Word8 -> Int -> Word8
    w8 base rshift = base .|. (fromIntegral (shiftR x rshift) .&. mask)
      where
      mask
        | base == 0x80 = 0x3F
        | base == 0xC0 = 0x1F
        | base == 0xE0 = 0x0F
        | base == 0xF0 = 0x07
        | base == 0xF8 = 0x03
        | base == 0xFC = 0x01

{-
And the rules for UTF8-to-codepoint:
examine first byte:
0x00-0x7F: 1 byte: as-is
0x7F-0xBF: error
0xC0-0xDF: 2 bytes: b1 AND 0x1F + remaining
0xE0-0xEF: 3 bytes: b1 AND 0x0F + remaining
0xF0-0xF7: 4 bytes: b1 AND 0x07 + remaining
0xF8-0xFB: 5 bytes: b1 AND 0x03 + remaining
0xFC-0xFD: 6 bytes: b1 AND 0x01 + remaining
0xFE-0xFF: error
  (byte-order-mark indicators: UTF8 - EFBBBF, UTF16 - FEFF or FFFE)
remaining = lower 6 bits of each byte, concatenated
-}

-- | Convert UTF-8 to Unicode.
fromUTF8 :: [Word8] -> String
fromUTF8 [] = ""
fromUTF8 (x:xs)
      | x <= 0x7F = remaining 0 (bAND x 0x7F) xs
      | x <= 0xBF = err x
      | x <= 0xDF = remaining 1 (bAND x 0x1F) xs
      | x <= 0xEF = remaining 2 (bAND x 0x0F) xs
      | x <= 0xF7 = remaining 3 (bAND x 0x07) xs
      | otherwise = err x
      -- Again, only works for chars > 0x0010FFFF, which we can't test.
      | x <= 0xFB = remaining 4 (bAND x 0x03) xs
      | x <= 0xFD = remaining 5 (bAND x 0x01) xs
      | otherwise = err x
  where
    err x = error ("fromUTF8: illegal UTF-8 character " ++ show x)
    bAND :: Word8 -> Word8 -> Int
    bAND x m = fromIntegral (x .&. m)
    remaining :: Int -> Int -> [Word8] -> String
    remaining 0 x xs = chr x : fromUTF8 xs
    remaining n x [] = error "fromUTF8: incomplete UTF8 sequence"
    remaining n x (b:xs)
      | b == 0 = err x
      | otherwise = remaining (n-1) ((shiftL x 6) .|. (bAND b 0x3F)) xs

-- | Convert UTF-8 to Unicode, from a C array of bytes.
-- This function is useful, in addition to 'fromUTF8' above,
-- because it doesn't create an intermediate @[Word8]@ list.
fromUTF8Ptr :: Ptr Word8 -> IO String
fromUTF8Ptr p = do
  x <- peek p
  case () of
    _ | x==0 -> return ""
      | x<=0x7F -> remaining 0 (bAND x 0x7F) (advance p)
      | x<=0xBF -> err x
      | x<=0xDF -> remaining 1 (bAND x 0x1F) (advance p)
      | x<=0xEF -> remaining 2 (bAND x 0x0F) (advance p)
      | x<=0xF7 -> remaining 3 (bAND x 0x07) (advance p)
      | otherwise -> err x
      -- Again, only works for chars > 0x0010FFFF, which we can't test.
      | x<=0xFB -> remaining 4 (bAND x 0x03) (advance p)
      | x<=0xFD -> remaining 5 (bAND x 0x01) (advance p)
      | otherwise -> err x
  where
    err x = error ("fromUTF8Ptr: illegal UTF-8 character " ++ show x)
    advance :: Ptr Word8 -> Ptr Word8
    advance p = plusPtr p 1
    bAND :: Word8 -> Word8 -> Int
    bAND x m = fromIntegral (x .&. m)
    remaining :: Int -> Int -> Ptr Word8 -> IO String
    remaining 0 x p = do
      s <- fromUTF8Ptr p
      return ((chr x):s)
    remaining n x p = do
      b <- peek p
      when (b == 0) (err x)
      remaining (n-1) ((shiftL x 6) .|. (bAND b 0x3F)) (advance p)
