{-# OPTIONS -fglasgow-exts  #-}

module Data.Types.Test.ReifyReflect where

import Data.Types.ReifyReflect
import qualified Debug.QuickCheck as QC
import Foreign.Storable (Storable)


---------- Integrals


vzero = undefined :: Zero
vone = undefined :: Succ Zero
vtwo = undefined :: Twice (Succ Zero)
vfour = undefined :: Twice (Twice (Succ Zero))

prop_reflectNumRoundTrip :: Int -> Bool
prop_reflectNumRoundTrip i = reifyIntegral i reflectNum == i

prop_reflectNumZero = reflectNum vzero == (0::Int)
prop_reflectNumOne = reflectNum vone == (1::Int)
prop_reflectNumTwo = reflectNum vtwo == (2::Int)
prop_reflectNumFour = reflectNum vfour == (4::Int)


prop_reflectNumsRoundTrip :: [Int] -> Bool
prop_reflectNumsRoundTrip ii = reifyIntegrals ii reflectNums == ii


---------- Storable

--prop_reflectStorableRoundTrip :: (Eq s, Storable s) => s -> Bool
prop_reflectStorableRoundTrip :: Double -> Bool
prop_reflectStorableRoundTrip s = reifyStorable s reflectStorable == s

-- | How do I test this with QuickCheck?
reflectStorableIORoundTrip :: Double -> IO Bool
reflectStorableIORoundTrip d = do
  r <- reifyStorableIO d reflectStorableIO
  return (r == d)

testReflectStorableIORoundTrip :: Double -> IO ()
testReflectStorableIORoundTrip d = do
  b <- reflectStorableIORoundTrip d
  if not b
    then putStrLn $ (show b) ++ " failed for reflectStorableRoundTrip"
    else return ()

testReflectStorableIORoundTrips :: IO ()
testReflectStorableIORoundTrips =
  let ds :: [Double]
      ds = [3.75, 1.1, 1.0, -483893.393]
  in mapM_ testReflectStorableIORoundTrip ds



---------- Arbitrary types


data NotStorable = NS1 | NS2 | NS3
  deriving (Eq, Show)

prop_reflectRoundTrip :: Int -> Bool
prop_reflectRoundTrip i = reify i reflect == i


reflectIORoundTrip :: NotStorable -> IO Bool
reflectIORoundTrip d = do
  r <- reifyIO d reflectIO
  return (r == d)

testReflectIORoundTrip :: NotStorable -> IO ()
testReflectIORoundTrip d = do
  b <- reflectIORoundTrip d
  if not b
    then putStrLn $ (show b) ++ " failed for reflectRoundTrip"
    else return ()

testReflectIORoundTrips :: IO ()
testReflectIORoundTrips =
  let ds :: [NotStorable]
      ds = [NS1, NS2, NS3]
  in mapM_ testReflectIORoundTrip ds


---------- Driver

runTests = do
  QC.quickCheck prop_reflectNumRoundTrip
  QC.quickCheck prop_reflectNumZero
  QC.quickCheck prop_reflectNumOne
  QC.quickCheck prop_reflectNumTwo
  QC.quickCheck prop_reflectNumsRoundTrip
  QC.quickCheck prop_reflectStorableRoundTrip
  testReflectStorableIORoundTrips
  QC.quickCheck prop_reflectRoundTrip
  testReflectIORoundTrips
