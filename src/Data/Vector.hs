{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Vector
  ( -- * Vector
    Vector,
    unsafeFreeze,

    -- * MVector
    MVector,
    empty,
    size,
    cells,
    push,
    push#
  )
where

import Control.Monad.Primitive (PrimMonad, PrimState, primitive)
import Data.Primitive.SmallArray (SmallArray (SmallArray), sizeofSmallArray)
import GHC.Base qualified as GHC
import GHC.Exts (Int (I#), Int#, IsList, SmallArray#, SmallMutableArray#, State#, fromList, toList)
import GHC.Exts qualified as GHC

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

data Box a = Box (# (# #)| a #)

instance Eq a => Eq (Box a) where
  Box (# (##) | #) == Box (# (##) | #) = True
  Box (# | x #) == Box (# | y #) = x == y
  _ == _ = False
  {-# INLINE (==) #-}

instance Show a => Show (Box a) where
  show (Box (# (##) | #)) = "Box Nil"
  show (Box (# | x #)) = "Box " ++ show x
  {-# INLINE show #-}

data Vector a = Vector (# Int#, SmallArray# (Box a) #)

instance IsList (Vector a) where
  type Item (Vector a) = a

  fromList xs = case fromList (map (\x -> Box (# | x #)) xs) of
    array@(SmallArray xs#) -> case sizeofSmallArray array of
      I# s# -> Vector (# s#, xs# #)
  {-# INLINE fromList #-}

  toList (Vector (# _, xs# #)) =
    foldr go [] (toList (SmallArray xs#))
    where
      go (Box (# (##) | #)) xs = xs
      go (Box (# | x #)) xs = x : xs
  {-# INLINE toList #-}

instance Show a => Show (Vector a) where
  show xs = "Vector " ++ show (toList xs) ++ ""
  {-# INLINE show #-}

unsafeFreeze :: PrimMonad m => MVector (PrimState m) a -> m (Vector a)
unsafeFreeze (MVector (# s#, xs0# #)) =
  primitive \st0# ->
    let !(# st1#, xs1# #) = GHC.unsafeFreezeSmallArray# xs0# st0#
     in (# st1#, Vector (# s#, xs1# #) #)

data MVector s a = MVector (# Int#, SmallMutableArray# s (Box a) #)

empty :: PrimMonad m => Int -> m (MVector (PrimState m) a)
empty (I# n#) =
  primitive \st0# ->
    let !(# st1#, xs# #) = GHC.newSmallArray# n# (Box (# (##) | #)) st0#
     in (# st1#, MVector (# 0#, xs# #) #)
{-# INLINE empty #-}

size :: MVector s a -> Int
size (MVector (# s#, _ #)) = I# s#
{-# INLINE size #-}

cells :: PrimMonad m => MVector (PrimState m) a -> m Int
cells (MVector (# _, xs0# #)) =
  primitive \st0# ->
    let !(# st1#, len# #) = GHC.getSizeofSmallMutableArray# xs0# st0#
     in (# st1#, I# len# #)
{-# INLINE cells #-}

push :: PrimMonad m => a -> MVector (PrimState m) a -> m (MVector (PrimState m) a)
push x vec = primitive (push# x vec)
{-# INLINE push #-}

push# :: a -> MVector s a -> State# s -> (# State# s, MVector s a #)
push# x (MVector (# s#, xs0# #)) st0# =
  let !(# st1#, len# #) = GHC.getSizeofSmallMutableArray# xs0# st0#
   in case s# GHC.<# len# of
        1# ->
          let !st2# = GHC.writeSmallArray# xs0# s# (Box (# | x #)) st1#
           in (# st2#, MVector (# 1# GHC.+# s#, xs0# #) #)
        _ ->
          let !(# st2#, xs1# #) = GHC.resizeSmallMutableArray# xs0# (2# GHC.*# len#) (Box (# (##) | #)) st1#
              !st3# = GHC.writeSmallArray# xs1# s# (Box (# | x #)) st2#
           in (# st3#, MVector (# 1# GHC.+# s#, xs1# #) #)
{-# INLINE push# #-}
