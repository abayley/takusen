> {-# OPTIONS -fglasgow-exts  #-}

> module Data.Types.ReifyReflect where


> import Data.Word (Word8)
> import Foreign.Storable (Storable, sizeOf, peek)
> import Foreign.Ptr (Ptr, castPtr)
> import Foreign.StablePtr (StablePtr, newStablePtr, freeStablePtr, deRefStablePtr)
> import Foreign.Marshal.Array (pokeArray, peekArray)
> import Foreign.Marshal.Utils (with)
> import Foreign.Marshal.Alloc (alloca)
> import System.IO.Unsafe


---------- Integrals ----------

> data Zero
> data Twice s
> data Succ s
> data Pred s

> class ReflectNum s where reflectNum :: Num a => s -> a

> instance ReflectNum Zero
>   where reflectNum _ = 0
> instance ReflectNum s => ReflectNum (Twice s)
>   where reflectNum _ = (reflectNum (undefined::s)) * 2
> instance ReflectNum s => ReflectNum (Succ s)
>   where reflectNum _ = (reflectNum (undefined::s)) + 1
> instance ReflectNum s => ReflectNum (Pred s)
>   where reflectNum _ = (reflectNum (undefined::s)) - 1


| reifyIntegral takes as the reflect function as its second argument.

> reifyIntegral :: Integral a => a -> (forall s. ReflectNum s => s -> w) -> w
> reifyIntegral i k = case quotRem i 2 of
>   (0,  0) -> k (undefined::Zero)
>   (j,  0) -> reifyIntegral j (\(_::s) -> k (undefined::Twice s))
>   (j,  1) -> reifyIntegral j (\(_::s) -> k (undefined::Succ(Twice s)))
>   (j, -1) -> reifyIntegral j (\(_::s) -> k (undefined::Pred(Twice s)))


---------- Lists of Numbers ----------

> data Nil
> data Cons s ss

> class ReflectNums ss where reflectNums :: Num a => ss -> [a]

> instance ReflectNums Nil
>   where reflectNums _ = []
> instance (ReflectNum s, ReflectNums ss) => ReflectNums (Cons s ss)
>   where reflectNums _ = reflectNum (undefined::s) : reflectNums (undefined::ss)


> reifyIntegrals :: Integral a => [a] -> (forall ss. ReflectNums ss => ss -> w) -> w
> reifyIntegrals [] k = k (undefined::Nil)
> reifyIntegrals (i:ii) k = reifyIntegral i (\(_::s) ->
>   reifyIntegrals ii (\(_::ss) -> k (undefined::Cons s ss)))


---------- Storable ----------

> type Byte = Word8
> data Store s a


| The "pure" version, using unsafePerformIO.

> class ReflectStorable s where reflectStorable :: Storable a => s a -> a

> instance ReflectNums s => ReflectStorable (Store s) where
>   reflectStorable _ = unsafePerformIO $ alloca $ \p -> do
>     pokeArray (castPtr p) bytes
>     peek p
>     where bytes = reflectNums (undefined::s) :: [Byte]

> reifyStorable :: Storable a => a -> (forall s. ReflectStorable s => s a -> w) -> w
> reifyStorable a k =
>   let bytes = unsafePerformIO $ with a (peekArray (sizeOf a) . castPtr)
>   in reifyIntegrals (bytes :: [Byte]) (\(_::s) ->
>     k (undefined::(Store s a)))


| In the IO Monad. This merely avoids the use of unsafePerformIO,
but the price is the inconvenience of being stuck in the IO Monad.

> class ReflectStorableIO s where reflectStorableIO :: Storable a => s a -> IO a

> instance ReflectNums s => ReflectStorableIO (Store s) where
>   reflectStorableIO _ = alloca $ \p -> do
>     pokeArray (castPtr p) bytes
>     peek p
>     where bytes = reflectNums (undefined::s) :: [Byte]

> reifyStorableIO :: Storable a => a -> (forall s. ReflectStorableIO s => s a -> IO w) -> IO w
> reifyStorableIO a k = do
>   bytes <- with a (peekArray (sizeOf a) . castPtr)
>   reifyIntegrals (bytes :: [Byte]) (\(_::s) ->
>     k (undefined::(Store s a)))



---------- Any value ----------


> data Stable (s:: * -> *) a

| "Pure" reflect, using unsafePerformIO.

> class Reflect s a | s -> a where reflect :: s -> a

> instance ReflectStorable s => Reflect (Stable s a) a where
>   reflect = unsafePerformIO $ do
>     let p = reflectStorable (undefined:: s p)
>     a <- deRefStablePtr p
>     freeStablePtr p
>     return (const a)

> reify :: a -> (forall s. Reflect s a => s -> w) -> w
> reify (a::a) k = unsafePerformIO $ do
>   p <- newStablePtr a
>   let k' (s::s) = (reflect::s -> a) `seq` return (k s)
>   reifyStorable p (\(_::s p) ->
>     k' (undefined::Stable s a))



| Reflect in the IO monad. Inconvenient, but avoids use of unsafePerformIO.

> class ReflectIO s a | s -> a where reflectIO :: s -> IO a

> instance ReflectStorableIO s => ReflectIO (Stable s a) a where
>   reflectIO _ = do
>     p <- reflectStorableIO (undefined:: s p)
>     a <- deRefStablePtr p
>     freeStablePtr p
>     return a

> reifyIO :: a -> (forall s. ReflectIO s a => s -> IO w) -> IO w
> reifyIO (a::a) k = do
>   p <- newStablePtr a
>   reifyStorableIO p (\(_::s p) ->
>     k (undefined::Stable s a))
