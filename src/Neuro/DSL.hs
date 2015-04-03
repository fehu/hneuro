module Neuro.DSL
( dsl
, Id, Delay, LinkDirection
, DSLResolved   (layers, connections)
, DSLLayer      (DSLLayer, iElems)
, DSLConnection (from, to, dir)

, DSLRaw        (NeuroNet)
, DSLFragment   (Layer)

, Elem (Neuron, In, Out, Delayed)
, ElemSel ()
, sel, sel', except
, (-->), (<--), all2all
) where

import Data.Map ( Map, member, fromList, size )
import Data.List ( partition )

--import Data.Foldable (toList)
--flatMap f = concatMap (Data.Foldable.toList . f)

--  -- DSL Elements
--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --
--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --

data DSLFragment = Layer [Elem]
                 | Link ElemSel ElemSel LinkDirection
                 deriving (Eq, Show)

data DSLRaw = NeuroNet [DSLFragment]

data DSLLayer = DSLLayer { iElems :: Map Id Elem }
data DSLConnection = DSLConnection { from :: Id
                                   , to :: Id
                                   , dir :: LinkDirection
                                   }
data DSLResolved = DSLRes { layers :: [DSLLayer]
                          , connections :: [DSLConnection]
                          }

data Elem = Neuron | In | Out | Delayed Delay
    deriving (Eq, Show)

type Delay = Int

type Id = (Int, Int)

data ElemSel = Sel       {layer :: Int, i      :: [Int]}
             | SelExcept {layer :: Int, selExcept :: [Int]}
             deriving (Eq, Show)

data LinkDirection = Forward | BackOrSame
                   deriving (Eq, Show)

sel     :: Int -> Int   -> ElemSel
sel'    :: Int -> [Int] -> ElemSel
except  :: Int -> [Int] -> ElemSel

(-->)   :: ElemSel -> ElemSel -> DSLFragment
(<--)   :: ElemSel -> ElemSel -> DSLFragment
all2all :: Int -> Int -> [Int] -> [Int] -> DSLFragment

dsl     :: DSLRaw -> DSLResolved

--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --
--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --


l `sel` n     = Sel l [n]
l `sel'` ns   = Sel l ns
l `except` ns = SelExcept l ns

determineDir from to = if layer from < layer to then Forward else BackOrSame

from --> to = if layer from < layer to then Link from to Forward
                                             else error "back-going link"

to <-- from = if layer from > layer to then Link from to BackOrSame
                                             else error "forward-going link"

all2all lFrom lTo exFrom exTo = Link from to dir
                              where from = lFrom `except` exFrom
                                    to   = lTo `except` exTo
                                    dir  = determineDir from to


isLayer (Layer _) = True
isLayer _         = False

listSel :: [DSLLayer] -> ElemSel -> [Id]
listSel layers (Sel l ns) = if all (\i -> member i $ iElems $ layers !! l) ids
                            then ids
                            else error ("one of " ++ show ids ++ " links to nonexistent element " ++ (show $ map iElems layers))
                          where ids = [(l, i) | i <- ns]

listSel layers (SelExcept l ns) = [ (l, i) | i <- filter (\x -> notElem x ns) [1.. size $ iElems $ layers !! l] ]


dsl (NeuroNet raw) = DSLRes layers $ foldr (++) [] conns
        where dparts = partition isLayer raw
              layers = map mkLayer $ zip (fst dparts) [0..]
              conns  = map mkConns (snd dparts)
              mkLayer (x, l) = case x of Layer xs    -> DSLLayer $ fromList $ map (\(e,i) -> ((l, i), e)) $ zip xs [1..]
              mkConns x = case x of Link from to dir -> [DSLConnection f t dir | f <- listSel layers from, t <- listSel layers to]

                --          -- --          -- --          -- --          -- --          -- --          --
                --          -- --          -- --          -- --          -- --          -- --          --
                --          -- --          -- --          -- --          -- --          -- --          --

tst = dsl $ NeuroNet $ Layer (replicate 5 In ++ [Delayed i | i <- [1..2]])
                     : Layer (replicate 10 Neuron)
                     : Layer (replicate 10 Neuron)
                     : Layer (replicate 3 Out)
                     : all2all 1 2 [] []
                     : all2all 3 4 [1, 10] []
                     : [ (2 `sel'` [1..10])   --> (4 `sel` 1)
                       , (2 `except` [5])     --> (3 `sel'` [1..10])
                       , (1 `sel` 6)          <-- (3 `sel` 1)
                       , (1 `sel` 7)          <-- (3 `sel` 2)
                       ]

