
-- |
-- Module      :  Foreign.C.Unicode
-- Copyright   :  (c) 2004 George Russell, Alistair Bayley
-- License     :  BSD-style
-- Maintainer  :  alistair@abayley.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions to convert various Unicode encodings
-- into Haskell Strings (at present we only handle UTF-8,
-- but we'd like to add UTF-16 and UTF-32).
--
-- We assume that the UTF-8 encoded String is a list of
-- Chars where, for each char c, 0 <= ord c <= 255.
-- When we convert a Haskell String into a UTF-8 string,
-- again the Chars in the resulting String are all
-- codepoints from 0 to 255.


module Foreign.C.Unicode
  ( peekUTF8String, newUTF8String, withUTF8String, withUTF8StringLen
  , toUTF8, fromUTF8, fromUTF8E, fromUTF8WE
  ) where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array
import Data.Char
import Data.List
import Data.Bits
import Control.Monad.Error


nullCChar :: CChar
nullCChar = 0

-- | Analogous to peekCString. Converts UTF8 CString to String.
peekUTF8String :: CString -> IO String
peekUTF8String cs = do
  s <- peekArray0 nullCChar cs
  fromUTF8 (map castCCharToChar s)

-- | Analogous to newCString. Creates UTF8 encoded CString.
newUTF8String :: String -> IO CString
newUTF8String hs =
  newArray0 nullCChar (map castCharToCChar (toUTF8 hs))

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
    pokeArray0 nullCChar ptr (map castCharToCChar utf8Str)
    action (ptr, lenUtf8)


-- --------------------------------------------------------------------------
-- Encoding
-- --------------------------------------------------------------------------

-- | Converts a String into its UTF8 representation.
toUTF8 :: String -> String
toUTF8 [] = []
toUTF8 (x:xs) =
    let
       xs1 = toUTF8 xs
       ox = ord x

       mkUTF8 :: Int -> String -> Int -> Int -> String
       mkUTF8 x0 xs0 xmask0 xmax0 =
          let
             xbot = 0x80 .|. (x0 .&. 0x3f)
             x1 = x0 `shiftR` 6
             xs1 = chr xbot : xs0
          in
             if x1 < xmax0
               then
                  chr (xmask0 .|. x1) : xs1
               else
                  let
                     xmask1 = xmask0 .|. xmax0
                     xmax1 = xmax0 `shiftR` 1
                  in
                     mkUTF8 x1 xs1 xmask1 xmax1
    in
       if ox <= 0x7f
          then
             x : xs1
          else
            if ox `shiftR` 31 /= 0
               then
                  error ("Huge character with code " ++ show ox ++
                     " detected in string being converted to UTF8.")
               else
                  mkUTF8 ox xs1 0xc0 0x20


-- | We really want a fromUTF8 that will do error recovery
-- i.e. it will never raise an error, and do the best job it can,
-- inserting substitution characters where necessary.
-- But for now we'll just pretend...

fromUTF8 :: Monad m => String -> m String
fromUTF8 = fromUTF8WE

-- | Instance of Control.Monad.MonadError, as suggested below.

fromUTF8E :: String -> Either String String
fromUTF8E = fromUTF8WE


-- | Converts a UTF8 representation of a String back into the String,
-- catching all possible format errors.
--
-- Example: With the Haskell module Control.Monad.Error, you can
-- instance this as
-- (fromUTF8WE :: String -> Either String String)
-- to get a conversion function which either succeeds (Right) or
-- returns an error message (Left).

fromUTF8WE :: Monad m => String -> m String
fromUTF8WE [] = return []
fromUTF8WE (x0 : xs0) =
    let
       ox = ord x0
    in
       case topZero8 ox of
          7 ->
             do
                xs1 <- fromUTF8WE xs0
                return (x0 : xs1)
          6 ->
             fail "UTF8 escape sequence starts 10xxxxxx"
          0 ->
             fail "UTF8 escape sequence starts 11111110"
          -1 ->
             fail "UTF8 escape sequence starts 11111111"
          n ->
             let
                r = 6 - n -- number of 6-bit pieces
                xtop = ox .&. ones n

                minx =
                   bit (
                      if r == 1
                         then
                            7
                         else
                            5*r + 1
                      )

                mkx [] _ _ =
                   fail "UTF8 string ends in middle of escape sequence"
                mkx (ch : xs1) x0 count0 =
                   do
                      let
                         och = ord ch
                      if och .&. 0x80 /= 0x80
                         then
                            fail ("UTF8 escape sequence contains continuing "
                               ++ "character not of form 10xxxxxx")
                         else
                            return ()
                      let
                         xbot = och .&. 0x3f
                         x1 = (x0 `shiftL` 6) .|. xbot
                         count1 = count0 - 1
                      if count1 == 0
                         then
                            return (x1,xs1)
                         else
                            mkx xs1 x1 count1
             in
                do
                   (x,xs1) <- mkx xs0 xtop r
                   if x < minx
                      then
                         fail ("UTF8 escape sequence contains character not "
                            ++ "optimally encoded")
                      else
                         do
                            xs2 <- fromUTF8WE xs1
                            return (chr x : xs2)


-- --------------------------------------------------------------------------
-- Binary utilities
-- --------------------------------------------------------------------------

-- | return the number of the top bit which is zero, or -1 if they
-- are all zero, for a number between 0 and 255.
topZero8 :: Int -> Int
topZero8 i =
    case
       (findIndex not
          (map
             (\ bn -> testBit i bn)
             [7,6..0]
             ))
       of
          Just n -> 7 - n
          Nothing -> -1

-- | (ones i) is number with binary representation 1 written i times.
ones :: Int -> Int
ones i = bit i
