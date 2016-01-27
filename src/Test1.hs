----------------------------------------------------------------------------
 ---- Examples ---- Examples ---- Examples ---- Examples ---- Examples ----
----------------------------------------------------------------------------

module Test1 ( test ) where

import Neuro.DSL

test = inputsLayer (undefined :: SomeNat Nat2)
    ==> nextLayer (  NeuronInputs (\(HCons il _) -> vecElem1 il +: vecElem2 il +: VNil)
                  +: NeuronInputs (\(HCons il _) -> vecElem1 il +: vecElem2 il +: VNil)
                  +: VNil
                  )
    ==> nextLayer (  NeuronInputs (\(HCons l1 _)            -> vecElem1 l1 +: vecElem2 l1 +: VNil)
                  +: NeuronInputs (\(HCons l1 (HCons il _)) -> vecElem1 il +: vecElem2 l1 +: VNil)
                  +: NeuronInputs (\(HCons l1 (HCons il _)) -> vecElem1 l1 +: vecElem2 il +: VNil)
                  +: VNil
                  )
    ==> lastLayer (  NeuronInputs (\(HCons l2 _) -> vecElem1 l2
                                                 +: vecElem2 l2
                                                 +: vecElem3 l2
                                                 +: VNil
                                  ) +: VNil
                  )
