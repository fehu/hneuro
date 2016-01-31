----------------------------------------------------------------------------
 ---- Examples ---- Examples ---- Examples ---- Examples ---- Examples ----
----------------------------------------------------------------------------

module Main ( main, test ) where

import Neuro.DSL
import Neuro.Show.Tikz
import qualified Data.ByteString.Char8 as BS

test = inputsLayer (undefined :: Nat' N2)
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

tNet = nnet test

main = do let Tikz s = nShow tNet
          BS.putStrLn s



