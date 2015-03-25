module Neuro
( NetworkElem(..)
, newNeuron, newInput, newOutput, newDelaySink, newDelayedInput
,  isNeuron,  isInput,  isOutput,  isDelaySink,  isDelayedInput
, NetworkLayer(..)
--, newLayer
, test
) where

import NamedFunc
import Data.Map (Map, keys, elems)

data NetworkElem a = Neuron {weights :: [a], transfer :: NamedFunc ([a] -> a)}
                   | Input a
                   | Output a
                   | DelaySink a Int (NetworkElem a)
                   | DelayedInput Int

newNeuron w s f         = Neuron w $ f `named` s
newInput next_value     = Input next_value
newDelayedInput x       = DelayedInput x
newOutput out           = Output out
newDelaySink x d s@(DelayedInput _) = DelaySink x d s

isNeuron (Neuron _ _)           = True
isNeuron _                      = False

isInput (Input _)               = True
isInput _                       = False

isOutput (Output _)             = True
isOutput _                      = False

isDelayedInput (DelayedInput _) = True
isDelayedInput _                = False

isDelaySink (DelaySink _ _ _)   = True
isDelaySink _                   = False


--isDelay = isDelayedInput || isDelaySink

instance Show a => Show (NetworkElem a) where
    show (Neuron {weights = w, transfer = f}) = "Neuron(" ++ show w ++ ", " ++ show f ++ ")"
    show (Input x)                  = "in "  ++ show x
    show (Output x)                 = "out " ++ show x
    show (DelaySink x delay link)   = show x ++ " -- delay " ++ show delay ++ " -->" ++ show link
    show (DelayedInput x)           = "delayed" ++ show x

-- --
type Layer a = Map (NetworkElem a) [NetworkLayer a]

data NetworkLayer a = InLayer     (Layer a)
                    | HiddenLayer (Layer a)
                    | OutLayer    (Layer a)


compatible :: Layer a -> (NetworkElem a -> Bool) -> Bool
layer `compatible` f = all tstf (keys layer)
                     where tstf = \k -> foldr (\f a -> f(k) || a) False tst
                           tst  = [f, isDelayedInput, isDelaySink]
--noNext :: Layer a -> Bool
noNext layer = all null $ elems layer

newLayer :: Layer a -> NetworkLayer a
newLayer layer | layer `compatible` isInput     = InLayer layer
               | layer `compatible` isNeuron    = HiddenLayer layer
               | layer `compatible` isOutput
                            && noNext layer     = OutLayer layer

--newLayer out [] = if all outCompatible out
--                  then OutLayer out
--                  else error "incompatible layer "
--                where outCompatible x = case x of Output _ -> True
--                                                  DelaySink _ _ _-> True
--                                                  _ -> False
--                                      Input _ -> True
--                                      DelayedInput _ -> True

--newLayer elems next | elems == [] = error "empty layer"
--                    | 1 = null

instance Show a => Show (NetworkLayer a) where
    show (InLayer x)     = "InLayer" ++ show x
    show (OutLayer x)    = "OutLayer" ++ show x
    show (HiddenLayer x) = "HiddenLayer" ++ show x

layerElems :: NetworkLayer a -> Layer a
layerElems (InLayer x) = x
layerElems (OutLayer x) = x
layerElems (HiddenLayer x) = x

--nextLayer :: NetworkLayer a -> Maybe [NetworkLayer a]
--nextLayer (InLayer _ next)      = Just next
--nextLayer (OutLayer e)          = Nothing
--nextLayer (HiddenLayer _ next)  = Just next

--instance Functor NetworkLayer where
--    fmap f (In elems next) = In (f elems) next

-- --

--out = newLayer (replicate 5 $ newOutput 0) []
test = do
    putStrLn $ show $ newNeuron [1,2] "foo" head
--    putStrLn $ show out
