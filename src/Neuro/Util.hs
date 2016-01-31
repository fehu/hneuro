-----------------------------------------------------------------------------
--
-- Module      :  Neuro.Util
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE GADTs
           , FlexibleContexts
           , FlexibleInstances
       #-}

module Neuro.Util (

  Nat(..)
, Nat0
, Nat1
, Nat2
, Nat3
, SomeNat(..)


, Vec(..)
, Vec1
, Vec2
, Vec3

, GenVec(..)
, (+:)
, vec2list

, VecElem(..)
, vecElem1
, vecElem2
, vecElem3

) where


-----------------------------------------------------------------------------
-- From https://downloads.haskell.org/~ghc/7.4.1/docs/html/users_guide/kind-polymorphism-and-promotion.html

data Nat = Ze | Su Nat

data Vec :: Nat -> * -> * where
  VNil  :: Vec Ze a
  VCons :: a -> Vec n a -> Vec (Su n) a


--nat2int :: Nat -> Int
--nat2int Ze = 0
--nat2int (Su x) = nat2int x + 1


class Nat2Integral (n :: Nat) where
    nat2int :: Integral i => SomeNat n -> i

instance Nat2Integral Ze where
    nat2int _ = 0

instance Nat2Integral n => Nat2Integral (Su n) where
    nat2int n = nat2int n + 1


--class NatValue (n :: Nat) where natValue :: SomeNat n -> n

--class NatToInt (n :: Nat) where nat2int :: SomeNat n -> Int

--instance NatToInt Ze where nat2int _ = 0
--instance (NatToInt s) => NatToInt (Su s) where nat2int _ = 1 + nat2int (undefined :: SomeNat s)

vec2list :: Vec n a -> [a]
vec2list (VCons h t) = h : vec2list t
vec2list VNil = []

--vecLength :: Vec n a -> Int
--vecLength = nat2int n

-----------------------------------------------------------------------------

instance Functor (Vec n) where fmap f (VCons h t) = VCons (f h) (fmap f t)

instance Foldable (Vec n) where foldr _ b0 VNil = b0
                                foldr f b0 (VCons a t) = let res = f a b0
                                                         in foldr f res t

class GenVec (n :: Nat) where genVec :: a -> SomeNat n ->  Vec n a

instance                GenVec Nat0     where genVec _ _ = VNil
instance (GenVec np) => GenVec (Su np)  where genVec a _ = a +: genVec a undefined


infixr 5 +:
(+:) :: a -> Vec n a -> Vec (Su n) a
(+:) = VCons

-----------------------------------------------------------------------------

type Nat0 = Ze
type Nat1 = Su Nat0
type Nat2 = Su Nat1
type Nat3 = Su Nat2

data SomeNat (n :: Nat)

type Vec1 a = Vec Nat1 a
type Vec2 a = Vec Nat2 a
type Vec3 a = Vec Nat3 a

-----------------------------------------------------------------------------

class VecElem (n :: Nat) (vn :: Nat) where vecElem :: SomeNat n -> Vec vn a -> a

instance VecElem Nat1 Nat1 where vecElem _ (VCons x _) = x

instance VecElem Nat1 Nat2 where vecElem _ (VCons x _) = x
instance VecElem Nat2 Nat2 where vecElem _ (VCons _ (VCons x _)) = x

instance VecElem Nat1 Nat3 where vecElem _ (VCons x _) = x
instance VecElem Nat2 Nat3 where vecElem _ (VCons _ (VCons x _)) = x
instance VecElem Nat3 Nat3 where vecElem _ (VCons _ (VCons _ (VCons x _))) = x


vecElem1 :: (VecElem Nat1 vn) => Vec vn a -> a
vecElem1 = vecElem (undefined :: SomeNat Nat1)

vecElem2 :: (VecElem Nat2 vn) => Vec vn a -> a
vecElem2 = vecElem (undefined :: SomeNat Nat2)

vecElem3 :: (VecElem Nat3 vn) => Vec vn a -> a
vecElem3 = vecElem (undefined :: SomeNat Nat3)

-----------------------------------------------------------------------------
