module Neuro
( NamedFunc -- move
, named -- move
, Show
, NetworkElem(..)
, newNeuron
, NetworkLayer(..)
, newLayer
, test
) where

data NamedFunc f = NamedFunc String f

named :: f -> String -> NamedFunc f
f `named` s = NamedFunc s f

instance Show (NamedFunc f) where show (NamedFunc n _) = n

-- data Network a = Neuron {weights :: [a], transfer :: NamedFunc ([a] -> a)} | Input a | Output a | Delay Int
-- instance Show a => Show (Network a) where show Neuron {weights = w, transfer = f} = "Neuron(" ++ show w ++ ", " ++ show f ++ ")"
-- newNeuron :: [a] -> String -> ([a] -> a) -> Neuron a
-- ?? data NetworkLayer a = In [NetworkElem a] [NetworkLayer a] | HiddenLayer [NetworkElem a] [NetworkLayer a] | Out [NetworkElem a]

-- --
data NetworkElem a = Neuron {weights :: [a], transfer :: NamedFunc ([a] -> a)}
                   | Input a
                   | Output a
                   | DelaySink a Int (NetworkElem a)
                   | DelayedInput Int

newNeuron w s f         = Neuron w $ f `named` s
newInput next_value     = Input next_value
newOutput out           = Output out

newDelaySink x delay to = case to of DelayedInput _ -> DelaySink x delay to

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
--                                      Input _ -> True
--                                      DelayedInput _ -> True
                                      _ -> False

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
