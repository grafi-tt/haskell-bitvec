{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, KindSignatures, TypeFamilies, TypeOperators, UndecidableInstances #-}

module Data.BitVec (
) where

import qualified Data.BitVec.Class as C
import GHC.TypeLits

type BitVec n v = (BitVecFun n ~ v, C.IsBitVec v)

type family BitVecFun (n :: Nat) :: Nat -> *
type instance BitVecFun n = BitVecSel (Selector n)

type family BitVecSel (c :: Counter) :: Nat -> *
type instance BitVecSel 'C0 = C.IntegerVec
type instance BitVecSel 'C1 = C.Word64Vec
type instance BitVecSel 'C2 = C.Word32Vec

type family Selector (n :: Nat) :: Counter
type instance Selector n = (n <=? 32) :>: (n <=? 64) :>: C0

data Counter = C0 | C1 | C2
infixr 5 :>:
type family (b :: Bool) :>: (c :: Counter) :: Counter
type instance 'False :>: 'C0 = 'C0
type instance 'True  :>: 'C0 = 'C1
type instance 'False :>: 'C1 = 'C1
type instance 'True  :>: 'C1 = 'C2
type instance 'False :>: 'C2 = 'C2
type instance 'True  :>: 'C2 = 'C2
