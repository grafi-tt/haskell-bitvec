{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, KindSignatures, ScopedTypeVariables, TypeFamilies, TypeOperators #-}

module Data.BitVec.Class (
  Size,
  IsBitVec (
    toIntegerVec, fromIntegerVec
  ),
  IntegerVec, Word64Vec, Word32Vec,
  append
) where

import GHC.TypeLits

import qualified Data.Bits as B
import Data.Word

unsafeFromInteger :: IsBitVec u => Integer -> u n
unsafeFromInteger = fromIntegerVec . IntegerVec
unsafeToInteger :: IsBitVec u => u n -> Integer
unsafeToInteger = unwrapInteger . toIntegerVec

type family Or (a :: Bool) (b :: Bool) :: Bool
type instance Or 'False 'False = 'False
type instance Or 'True  'False = 'True
type instance Or 'False 'True  = 'True
type instance Or 'True  'True  = 'True

type (m :: Nat) ~<= (n :: Nat) = Or (m <=? n) (m <=? 0) ~ True

class IsBitVec (v :: Nat -> *) where
  type Size v :: Nat
  toIntegerVec :: v n -> IntegerVec n
  fromIntegerVec :: IntegerVec n -> v n

append :: forall t u v l m. (SingI (Size u), IsBitVec t, IsBitVec u, IsBitVec v, (l + m) ~<= Size v) => t l -> u m -> v (l + m)
append t u = unsafeFromInteger $
               unsafeToInteger t `B.unsafeShiftL` (fromInteger $ fromSing (sing :: Sing (Size u))) B..|. unsafeToInteger u

newtype IntegerVec (n :: Nat) = IntegerVec { unwrapInteger :: Integer }
instance IsBitVec IntegerVec where
  type Size IntegerVec = 0
  toIntegerVec = id
  fromIntegerVec = id

newtype Word64Vec (n :: Nat) = Word64Vec { unwrapWord64 :: Word64 }
instance IsBitVec Word64Vec where
  type Size Word64Vec = 64
  toIntegerVec = IntegerVec . toInteger . unwrapWord64
  fromIntegerVec = Word64Vec . fromInteger . unwrapInteger

newtype Word32Vec (n :: Nat) = Word32Vec { unwrapWord32 :: Word32 }
instance IsBitVec Word32Vec where
  type Size Word32Vec = 32
  toIntegerVec = IntegerVec . toInteger . unwrapWord32
  fromIntegerVec = Word32Vec . fromInteger . unwrapInteger
