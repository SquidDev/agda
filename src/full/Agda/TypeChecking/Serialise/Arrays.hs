{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
module Agda.TypeChecking.Serialise.Arrays
  ( ValueArray(valueArray)
  , NodeArray(nodeArray)
  , pattern NodeArr
  , pattern NodeAr1
  ) where

import Control.Monad.Trans
import Control.Monad.ST.Trans
import Control.Monad.ST.Trans.Internal (liftST)

import Data.Array.IArray
import Data.Array.Unboxed
import Data.Array.Base
import Data.Array.IO
import Data.Int
import Data.Proxy
import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B

import GHC.Generics

import Agda.Utils.Impossible

-- | An array of deserialised values.
--
-- This has the same binary representation as a list, but is more efficient to
-- deserialise.
newtype ValueArray a = ValueArray { valueArray :: Array Int32 a }

instance B.Binary a => B.Binary (ValueArray a) where
  put = __IMPOSSIBLE__ -- Will never serialise this
  get = fmap ValueArray $ runSTArray $ do
    n <- lift (B.get :: B.Get Int)
    arr <- newArray_ (0, fromIntegral n - 1) :: STT s B.Get (STArray s Int32 a)

    -- We'd like to use 'for_ [0..n-1]' here, but unfortunately GHC doesn't unfold
    -- the list construction and so performs worse than the hand-written version.
    let
      getMany i = if i == n then return () else do
        x <- lift B.get
        unsafeWriteSTArray arr i x
        getMany (i + 1)
    () <- getMany 0

    return arr

-- | An 0-indexed array of `Int32's, describing a node in the original graph.
newtype NodeArray = NodeArray { nodeArray :: UArray Int Int32 }

instance Show NodeArray where
  show = show . elems . nodeArray

instance B.Binary NodeArray where
  put = __IMPOSSIBLE__ -- Will never serialise this
  get = fmap NodeArray $ runSTUArray $ do
    n <- lift (B.get :: B.Get Int)
    arr <- newArray_ (0, n - 1) :: STT s B.Get (STUArray s Int Int32)

    let
      getMany i = if i == n then return () else do
        x <- lift B.get
        unsafeWrite arr i x
        getMany (i + 1)
    () <- getMany 0

    return arr

-- | Decompose a `NodeArray' to a tuple.
pattern NodeArr :: IntTuple a => a -> NodeArray
pattern NodeArr x <- (ofArray . nodeArray -> Just x)

-- | Decompose a `NodeArray' to a single value.
pattern NodeAr1 :: Int32 -> NodeArray
pattern NodeAr1 x <- (ofArray . nodeArray -> Just (x :: Int32))

-- | A tuple of integers, which can be produced from an array.
class IntTuple a where
  -- | Attempt to decompose an array, converting it into a tuple. The length of the array
  -- and target type must match exactly.
  ofArray :: UArray Int Int32 -> Maybe a

  {-# INLINE ofArray #-}
  default ofArray :: (Generic a, GIntTuple (Rep a)) => UArray Int Int32 -> Maybe a
  ofArray xs
    | numElements xs == gSize (Proxy :: Proxy (Rep a)) = Just $ to (gOfArray 0 xs)
    | otherwise = Nothing

instance IntTuple Int32 where
  ofArray xs
    | numElements xs == 1 = Just (unsafeAt xs 0)
    | otherwise = Nothing

-- | A generic version of `IntTuple', which is used for deriving `IntTuple'
class GIntTuple f where
  gSize :: Proxy f -> Int
  gOfArray :: Int -> UArray Int Int32 -> f p

instance GIntTuple U1 where
  gSize _ = 0
  gOfArray _ _ = U1

instance (GIntTuple f, GIntTuple g) => GIntTuple (f :*: g) where
  gSize _ = gSize (Proxy :: Proxy f) + gSize (Proxy :: Proxy g)
  gOfArray offset arr = gOfArray offset arr :*: gOfArray (offset + gSize (Proxy :: Proxy f)) arr

instance c ~ Int32 => GIntTuple (K1 i c) where
  gSize _ = 1
  gOfArray offset arr = K1 (unsafeAt arr offset)

instance GIntTuple f => GIntTuple (M1 i t f) where
  gSize _ = gSize (Proxy :: Proxy f)
  gOfArray offset arr = M1 (gOfArray offset arr)

instance IntTuple ()
instance (a ~ Int32, b ~ Int32) => IntTuple (a, b)
instance (a ~ Int32, b ~ Int32, c ~ Int32) => IntTuple (a, b, c)
instance (a ~ Int32, b ~ Int32, c ~ Int32, d ~ Int32) => IntTuple (a, b, c, d)

runSTUArray :: (Ix i, Monad m) => (forall s . STT s m (STUArray s i e)) -> m (UArray i e)
runSTUArray st = runSTT (st >>= (liftST . unsafeFreezeSTUArray))
