module Neuro
( NetworkElem(..)
, newNeuron, newInput, newOutput, newDelaySink, newDelayedInput
,  isNeuron,  isInput,  isOutput,  isDelaySink,  isDelayedInput
, NetworkLayer(..)
, newLayer
, test
) where

import NamedFunc

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
data NetworkLayer a = InLayer     [NetworkElem a] [NetworkLayer a]
                    | HiddenLayer [NetworkElem a] [NetworkLayer a]
                    | OutLayer    [NetworkElem a]

newLayer :: [NetworkElem a] -> [NetworkLayer a] -> NetworkLayer a
newLayer out [] = if all outCompatible out
                  then OutLayer out
                  else error "incompatible layer "
                where outCompatible x = case x of Output _ -> True
                                                  DelaySink _ _ _-> True
                                                  _ -> False
--                                      Input _ -> True
--                                      DelayedInput _ -> True


--layerCompatible layer f = all ff layer
--                        where ff = \x -> f(x) || case x of

--newLayer elems next | elems == [] = error "empty layer"
--                    | 1 = null

instance Show a => Show (NetworkLayer a) where
    show (InLayer  elems _)     = "InLayer" ++ show elems
    show (OutLayer elems)       = "OutLayer" ++ show elems
    show (HiddenLayer  elems _) = "HiddenLayer" ++ show elems

layerElems :: NetworkLayer a -> [NetworkElem a]
layerElems (InLayer e _) = e
layerElems (OutLayer e) = e
layerElems (HiddenLayer e _) = e

nextLayer :: NetworkLayer a -> Maybe [NetworkLayer a]
nextLayer (InLayer _ next)      = Just next
nextLayer (OutLayer e)          = Nothing
nextLayer (HiddenLayer _ next)  = Just next

--instance Functor NetworkLayer where
--    fmap f (In elems next) = In (f elems) next

-- --

out = newLayer (replicate 5 $ newOutput 0) []
test = do
    putStrLn $ show $ newNeuron [1,2] "foo" head
    putStrLn $ show out
