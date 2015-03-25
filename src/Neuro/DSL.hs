module Neuro.DSL
( Elem
, Layer(..)
, (-->)
, (+:+)
, all2all
, test
) where

import Data.Map (Map, fromList, assocs)

--  -- DSL Elements

data Elem = Neuron | In | Out | Delayed Int | Delay Int
    deriving (Eq, Show)

data Layer = Layer [Elem] deriving (Eq, Show)

data Connections = Connections [Link] deriving Show
(+:+) :: Connections -> Connections -> Connections

data ElemSel = Sel {layer :: Int, i :: [Int]}
             deriving (Eq, Show)

sel  :: Int -> Int   -> ElemSel
sel' :: Int -> [Int] -> ElemSel

type Id = Int
type LastId = Id

type ZippedId a = [(Id, a)]
type ZuppedIdL a = (ZippedId a, LastId)

type IdentifiedLayer = Map Id Elem

data Link = HardLink (Id, Elem) (Id, Elem)
          | WeakLink ElemSel ElemSel
          deriving (Eq, Show)

(-->)   :: ElemSel -> ElemSel -> Link
        --                             layer from  layer to    except from   except to
all2all :: IdentifiedNeuroNetStruct -> Int       -> Int     -> [Int]      -> [Int] -> Connections


data ANeuroNetStruct a = ANeuroNetStruct { inputs :: a
                                         , hidden :: [a]
                                         , outputs :: a
                       } deriving Show


type NeuroNetStruct = ANeuroNetStruct Layer

neuroNetStructure :: Layer -> [Layer] -> Layer -> NeuroNetStruct

zipId  :: LastId ->  [a]  -> ZuppedIdL a
zzipId :: LastId -> [[a]] -> ([ZippedId a], LastId)

type IdentifiedNeuroNetStruct = ANeuroNetStruct IdentifiedLayer
identifyNeuroNetStruct :: LastId -> NeuroNetStruct -> (IdentifiedNeuroNetStruct, LastId)


--  --
getLayer (Layer xs) = xs

getILayer :: Int -> IdentifiedNeuroNetStruct -> IdentifiedLayer
getILayer 0 (ANeuroNetStruct inp _ _)   = inp
getILayer i (ANeuroNetStruct _ hid out) = if i <= length hid
                                          then hid !! (i-1)
                                          else if i == 1 + length hid
                                               then out
                                          else error "out of range"

(Connections c1) +:+ (Connections c2) = Connections (c1 ++ c2)

l `sel`  i  = Sel l [i]
l `sel'` is = Sel l is

from --> to = WeakLink from to

--a@(Delay _) --> b@(Delayed _)   = Link a b
--(Delay _) --> _                 = error "Delay can only be connected to Delayed"
--a --> b                         = Link a b

all2all inet from to exceptFrom exceptTo = Connections [HardLink a b | a <- fFrom, b <- fTo]
                                         where fFrom = fb exceptFrom $ assocs $ getILayer from inet
                                               fTo   = fb exceptTo   $ assocs $ getILayer to inet
                                               ff e  = \(_,x) -> not $ x `elem` e
                                               fb e l = map fst $ filter (ff e) $ zip l [0..]
--                                                      where ll = map fst assocs $ getILayer from inet

joinNetStruct (ANeuroNetStruct i h o) = [i] ++ h ++ [o]

neuroNetStructure inp hid out   = ANeuroNetStruct inp hid out

zipId i xs = (zipped, i+len+1)
           where len = length xs
                 zipped = zip [i..] xs

zzipId i xss = foldr f ([], i) xss
             where f xs (ixs, ii) = (ixs ++ [fst r], snd r)
                                  where r = zipId ii xs
--             do (nxs, ni) <- zipId i xs
--                                      (nxs:ixs, ni)


--withZippedId :: (a -> b) -> ZuppedIdL a -> ZuppedIdL b
--withZippedId f (xs, z) = (map (\(i, x) -> (i, f x)) xs, z)

identifyNeuroNetStructFoldF f (acc, i) = do
    (xs, n) <- f i
    (acc ++ xs, n)

identifyNeuroNetStruct i nnet = (anet, ni)
                              where anet = ANeuroNetStruct (head l) ((init . tail) l) (last l)
                                    j   = joinNetStruct nnet
                                    zzipped = zzipId i $ map getLayer j
                                    l   = map fromList $ fst zzipped
                                    ni  = snd zzipped



------------------- Tst -------------------

ilayer = Layer [In, Delayed 1, In, Delayed 2, In]
layer1 = Layer [Neuron, Neuron, Neuron, Delayed 3]
layer2 = Layer $ replicate 10 Neuron
ulayer = Layer (replicate 2 Out ++ [Delay i | i <- [1..3]])

struct = neuroNetStructure ilayer [layer1, layer2] ulayer
istruct = identifyNeuroNetStruct 0 struct

all2all' = all2all $ fst istruct

connections = (all2all' 0 1 [] [])
          +:+ (all2all' 1 2 [] [0, 1, 8, 9])
          +:+ (Connections [])


test :: IO ()
test = putStrLn $ show connections

--Connections [
--
--]
