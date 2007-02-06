
-- |
-- Module      :  Foreign.C.Test.UTF8
-- Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
-- License     :  BSD-style
-- Maintainer  :  alistair@abayley.org
-- Stability   :  experimental
-- Portability :  non-portable
-- 
-- UTF8 decoder and encoder tests.

module Foreign.C.Test.UTF8 where

import Control.Exception
import Data.Char
import Foreign.C.String
import Foreign.C.UTF8
import Foreign.Marshal.Array
import Foreign.Ptr
import Test.MiniUnit
import qualified Test.QuickCheck as QC
import Word (Word8)


runTest :: IO ()
runTest = do
  counts <- runTestTT "Util module tests" testlist
  return ()

testlist = 
  [ testUTF8RoundTrip
  , quickCheckUTF8RoundTrip
  , testFromUTF8Failure5Bytes
  , testFromUTF8Failure6Bytes
  ]


instance QC.Arbitrary Char where
  arbitrary = QC.choose (chr 1, chr 0x10FFFF)
  coarbitrary = undefined

prop_roundtrip s = s == fromUTF8 (toUTF8 s)
quickCheckUTF8RoundTrip = QC.test prop_roundtrip

testUTF8RoundTrip = do
  utf8RoundTrip "1ByteLow"   0x000001 [0x01]
  utf8RoundTrip "1ByteHigh"  0x00007F [0x7F]
  utf8RoundTrip "2BytesLow"  0x000080 [0xC2, 0x80]
  utf8RoundTrip "2BytesHigh" 0x0007FF [0xDF, 0xBF]
  utf8RoundTrip "3BytesLow"  0x000800 [0xE0, 0xA0, 0x80]
  utf8RoundTrip "3BytesHigh" 0x00FFFF [0xEF, 0xBF, 0xBF]
  utf8RoundTrip "4BytesLow"  0x010000 [0xF0, 0x90, 0x80, 0x80]
  -- chr 0x10FFFF is the largest code-point Haskell currently allows
  utf8RoundTrip "4BytesHigh" 0x10FFFF [0xF4, 0x8F, 0xBF, 0xBF]

utf8RoundTrip :: String -> Int -> [Word8] -> IO ()
utf8RoundTrip msg unicode utf8 = do
  assertEqual ("testToUTF8-" ++ msg) utf8 (toUTF8 [chr unicode])
  assertEqual ("testFromUTF8-" ++ msg) [chr unicode] (fromUTF8 utf8)
  withUTF8String [chr unicode] $ \cstr -> do
    w8 <- peekArray0 0 (castPtr cstr)
    assertEqual ("testToUTF8Ptr-" ++ msg) utf8 w8
  withCString (map (chr . fromIntegral) utf8) $ \cstr -> do
    s <- peekUTF8String cstr
    assertEqual ("testFromUTF8Ptr-" ++ msg) [chr unicode] s

testFromUTF8Failure5Bytes = do
  let utf8 = [0xF8, 0x80, 0x80, 0x80, 0x80]
  catchJust errorCalls (do
    assertEqual "testFromUTF8Failure5Bytes" [chr 0x10FFFF] (fromUTF8 utf8)
    assertBool "testFromUTF8Failure5Bytes: no error raised" False
    ) (\msg -> do
      assertEqual "testFromUTF8Failure5Bytes" "fromUTF8: illegal UTF-8 character 248" msg
    )

testFromUTF8Failure6Bytes = do
  let utf8 = [0xFD, 0xBF, 0xBF, 0xBF, 0xBF, 0xBF]
  catchJust errorCalls (do
    assertEqual "testFromUTF8Failure6Bytes" [chr 0x10FFFF] (fromUTF8 utf8)
    assertBool "testFromUTF8Failure6Bytes: no error raised" False
    ) (\msg -> do
      assertEqual "testFromUTF8Failure6Bytes" "fromUTF8: illegal UTF-8 character 253" msg
    )
