{-# LANGUAGE ExistentialQuantification
         #-}

-----------------------------------------------------------------------------
--
-- Module      :  N2
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

module N2 where



class NeuralNetwork nn v where
    type NNInput  nn v :: *
    type NNOutput nn v :: *

    nnOutput :: nn -> NNInput nn v -> NNOutput nn v



-----------------------------------------------------------------------------

data NetInput n = NetInput (IO n)
--data NetOutput n = NetOutput (n -> IO())

class Neuron a i where neuronOutput :: a n -> i n -> n

data SomeNeuron i n = forall a . Neuron a i => SomeNeuron (a (i n))

data Synapse a b n = Synapse { sFrom :: a n, sTo :: b n }

-----------------------------------------------------------------------------

--data Nat = NextNat Nat
--         | Zero
--
--type Nat1 = NextNat Zero
--type Nat2 = NextNat Nat1
--type Nat3 = NextNat Nat2
--type Nat4 = NextNat Nat3
--type Nat5 = NextNat Nat4
--
--data SizedList a (n :: Nat) = SizedList [a]
--
--type SNil a = SizedList a Zero
--
--sNil :: a -> SizedList a Zero
--sNil a = SizedList []
--
--(:::) :: a -> SizedList a n -> NextNat n

-----------------------------------------------------------------------------

--type family SizedInput (a :: Nat) :: (* -> *)



--type instance SizedInput Nat1 =














