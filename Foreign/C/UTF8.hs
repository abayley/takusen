
-- |
-- Module      :  Foreign.C.UTF8
-- Copyright   :  (c) 2004 John Meacham, Alistair Bayley
-- License     :  BSD-style
-- Maintainer  :  alistair@abayley.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Marshall Haskell Strings to and from UTF8-encoded CStrings.

module Foreign.C.UTF8
  ( peekUTF8String, newUTF8String, withUTF8String, withUTF8StringLen
  , toUTF8, fromUTF8
  , toUTF8String, fromUTF8String
  ) where

import Data.Bits
import Data.Char
import Data.Word (Word8)
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Array

nullCChar :: CChar
nullCChar = 0

nullByte :: Word8
nullByte = 0

-- | Analogous to peekCString. Converts UTF8 CString to String.
peekUTF8String :: CString -> IO String
peekUTF8String cs = do
  s <- peekArray0 nullByte (castPtr cs)
  return (fromUTF s)

-- | Analogous to newCString. Creates UTF8 encoded CString.
newUTF8String :: String -> IO CString
newUTF8String hs = do
  p <- newArray0 nullByte (toUTF hs)
  return (castPtr p)

-- | Analogous to withCString. Creates UTF8 encoded CString.
withUTF8String :: String -> (CString -> IO a) -> IO a
withUTF8String s action =
  withUTF8StringLen s $ \(cstr, _) -> action cstr

-- | Analogous to withCStringLen.
withUTF8StringLen :: String -> (CStringLen -> IO a) -> IO a
withUTF8StringLen s action = do
  let utf8Str = toUTF s
  let lenUtf8 = length utf8Str
  allocaArray0 lenUtf8 $ \ptr -> do
    pokeArray0 nullByte ptr utf8Str
    action (castPtr ptr, lenUtf8)


toUTF8 = toUTF
fromUTF8 = fromUTF

-- | Convert a String that was marshalled from a CString without
-- any decoder applied. This might be useful if the client encoding
-- is unknown, and the user code must convert.
-- We assume that the UTF8 CString was marshalled as if Latin-1
-- i.e. all chars are in the range 0-255.
fromUTF8String = fromUTF . map charToWord8

-- | Convert a Haskell String into a UTF8 String, where each UTF8 byte
-- is represented by its Char equivalent i.e. only chars 0-255 are used.
-- The resulting String can be marshalled to CString directly i.e. with
-- a Latin-1 encoding.
toUTF8String = map word8ToChar . toUTF

charToWord8 :: Char -> Word8
charToWord8 = fromIntegral . fromEnum

word8ToChar :: Word8 -> Char
word8ToChar = toEnum . fromIntegral


-- | Convert Unicode characters to UTF-8.
toUTF :: String -> [Word8]
toUTF [] = []
toUTF (x:xs) | ord x<=0x007F = (fromIntegral $ ord x):toUTF xs
	     | ord x<=0x07FF = fromIntegral (0xC0 .|. ((ord x `shift` (-6)) .&. 0x1F)):
			       fromIntegral (0x80 .|. (ord x .&. 0x3F)):
			       toUTF xs
	     | otherwise     = fromIntegral (0xE0 .|. ((ord x `shift` (-12)) .&. 0x0F)):
			       fromIntegral (0x80 .|. ((ord x `shift` (-6)) .&. 0x3F)):
			       fromIntegral (0x80 .|. (ord x .&. 0x3F)):
			       toUTF xs

-- | Convert UTF-8 to Unicode.

fromUTF :: [Word8] -> String
fromUTF xs = fromUTF' (map fromIntegral xs) where
    fromUTF' [] = []
    fromUTF' (all@(x:xs))
	| x<=0x7F = (chr (x)):fromUTF' xs
	| x<=0xBF = err
	| x<=0xDF = twoBytes all
	| x<=0xEF = threeBytes all
	| otherwise   = err
    twoBytes (x1:x2:xs) = chr  ((((x1 .&. 0x1F) `shift` 6) .|.
			       (x2 .&. 0x3F))):fromUTF' xs
    twoBytes _ = error "fromUTF: illegal two byte sequence"

    threeBytes (x1:x2:x3:xs) = chr ((((x1 .&. 0x0F) `shift` 12) .|.
				    ((x2 .&. 0x3F) `shift` 6) .|.
				    (x3 .&. 0x3F))):fromUTF' xs
    threeBytes _ = error "fromUTF: illegal three byte sequence"

    err = error "fromUTF: illegal UTF-8 character"
