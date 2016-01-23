{-# LANGUAGE ExistentialQuantification
           , TypeSynonymInstances
           , FlexibleInstances
--           , ConstraintKinds
         #-}

-----------------------------------------------------------------------------
--
-- Module      :  N2.DSL
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--


module N2.DSL (

) where

import N2

import Data.Typeable
import Data.FixedList
--import qualified Data.FixedList as FL

data TODO

-----------------------------------------------------------------------------

data LayerType = InputLayer
               | HiddenLayer
               | OutputLayer

---- Cannot be empty
--data Layer a l = Layer LayerType (Cons l a) deriving Typeable
--
--data SomeLayer = forall a l . SomeLayer (Layer a l)
--
--data ANNetwork l = ANNetwork (Cons l SomeLayer)




-----------------------------------------------------------------------------

-- Cannot be empty
data Layer (t :: LayerType) a l = Layer (l a) deriving Typeable

data SomeLayer (t :: LayerType) = forall a l . SomeLayer (Layer t a l)
data SomeCLayer (t :: LayerType) l = forall a . SomeCLayer (Layer t a l)

data ANNetwork inputs hidden outputs = ANNetwork (SomeCLayer InputLayer inputs)
                                                 (hidden (SomeLayer HiddenLayer))
                                                 (SomeCLayer OutputLayer outputs)


-----------------------------------------------------------------------------

data NInput = NInput
data NOutput = NOutput


--type family NeuronByLayer (t :: LayerType) :: *
--
--type instance NeuronByLayer InputLayer = NInput
--type instance NeuronByLayer OutputLayer = NOutput



--type family LayerByNElem t :: LayerType
--
--type instance LayerByNElem NInput = InputLayer

-----------------------------------------------------------------------------

class (FixedList f) => GenFixedList f where
    genFixedList :: a -> f a

instance GenFixedList FixedList1 where genFixedList a = a :. Nil
instance GenFixedList FixedList2 where genFixedList a = a :. genFixedList a
instance GenFixedList FixedList3 where genFixedList a = a :. genFixedList a

-----------------------------------------------------------------------------


inputLayer :: (GenFixedList l) => SomeCLayer InputLayer l
inputLayer = SomeCLayer . Layer $ genFixedList NInput

outputLayer :: (GenFixedList l) => SomeCLayer OutputLayer l
outputLayer = SomeCLayer . Layer $ genFixedList NOutput

type InputLayer' l  = SomeCLayer InputLayer l
type OutputLayer' l = SomeCLayer OutputLayer l


describeANN :: ( GenFixedList i
               , GenFixedList h
               , GenFixedList o) => ANNetwork i h o
describeANN = ANNetwork inputLayer
                        undefined
                       outputLayer

--nnet = ANNetwork (inputLayer :: InputLayer' FixedList2)
--                 undefined
--                 (outputLayer :: OutputLayer' FixedList2)


--nnet :: ANNetwork FixedList2 FixedList1 FixedList2
--nnet = ANNetwork inputLayer
--                 undefined
--                 outputLayer

nnet = describeANN :: ANNetwork FixedList2 FixedList1 FixedList2

-----------------------------------------------------------------------------












