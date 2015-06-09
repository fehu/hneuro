module Neuro.DSL
( dsl
, Id, Delay, LinkDirection
, DSLResolved   (layers, connections)
, DSLLayer      (neuronIds)
, DSLDelayed    (DSLDelayed, delayedId, delayedDelay)
, DSLConnection (from, to, dir)

, DSLRaw        (NeuroNet)
, DSLFragment   (Layer)

, ElemSel ()
, sel, sel', except, delayed
, (-->), (<--), all2all
) where

import Data.Map ( Map, fromList )
import Data.List ( partition )

--import Data.Foldable (toList)
--flatMap f = concatMap (Data.Foldable.toList . f)

--  -- DSL Elements
--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --
--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --

data DSLFragment = Layer { size :: Int }
                 | Delayed { delay :: Int }
                 | Link ElemSel ElemSel LinkDirection
                 deriving (Eq, Show)

data DSLRaw = NeuroNet [DSLFragment]

data DSLLayer = DSLLayer { neuronIds :: [Id] }
data DSLDelayed = DSLDelayed { delayedId :: Int, delayedDelay :: Delay }
data DSLConnection = DSLConnection { from :: Id
                                   , to :: Id
                                   , dir :: LinkDirection
                                   }
data DSLResolved = DSLRes { layers :: [DSLLayer]
                          , delayedInputs :: [DSLDelayed]
                          , connections :: [DSLConnection]
                          }
type Delay = Int

type Id = (Int, Int)

data ElemSel = Sel        {layer :: Int, i         :: [Int]}
             | SelExcept  {layer :: Int, selExcept :: [Int]}
             | SelDelayed {n :: Int}
             deriving (Eq, Show)

data LinkDirection = Forward | BackOrSame
                   deriving (Eq, Show)

sel     :: Int -> Int   -> ElemSel
sel'    :: Int -> [Int] -> ElemSel
except  :: Int -> [Int] -> ElemSel
delayed :: Int -> ElemSel

(-->)   :: ElemSel -> ElemSel -> DSLFragment
(<--)   :: ElemSel -> ElemSel -> DSLFragment
all2all :: Int -> Int -> [Int] -> [Int] -> DSLFragment

dsl     :: DSLRaw -> DSLResolved

--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --
--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --


l `sel` n     = Sel l [n]
l `sel'` ns   = Sel l ns
l `except` ns = SelExcept l ns

delayed n     = SelDelayed n

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

isDelayed (Delayed _) = True
isDelayed _           = False

listSel :: [DSLLayer] -> ElemSel -> [Id]
listSel layers (Sel l ns) = if all (\i -> elem i $ neuronIds $ layers !! l) ids
                            then ids
                            else error ("one of " ++ show ids ++ " links to nonexistent element " ++ (show $ map neuronIds layers))
                          where ids = [(l, i) | i <- ns]

listSel layers (SelExcept l ns) = [ (l, i) | i <- filter (\x -> notElem x ns) [1.. length $ neuronIds $ layers !! l] ]


dsl (NeuroNet raw) = DSLRes layers delayd $ foldr (++) [] conns
        where dparts = partition isLayer raw
              ddparts = partition isDelayed $ snd dparts
              layers = map mkLayer $ zip (fst dparts) [0..]
              conns  = map mkConns (snd ddparts)
              delayd = map mkDelayd $ zip (fst ddparts) [1..]
              mkLayer (x, l) = case x of Layer n     -> DSLLayer $ zip (repeat l) [1..n]
              mkConns x = case x of Link from to dir -> [DSLConnection f t dir | f <- listSel layers from, t <- listSel layers to]
              mkDelayd (x, i) = case x of Delayed d -> DSLDelayed i d

                --          -- --          -- --          -- --          -- --          -- --          --
                --          -- --          -- --          -- --          -- --          -- --          --
                --          -- --          -- --          -- --          -- --          -- --          --

--tst = dsl $ NeuroNet $ Layer (replicate 5 In ++ [Delayed i | i <- [1..2]])
--                     : Layer (replicate 10 Neuron)
--                     : Layer (replicate 10 Neuron)
--                     : Layer (replicate 3 Out)
--                     : all2all 1 2 [] []
--                     : all2all 3 4 [1, 10] []
--                     : [ (2 `sel'` [1..10])   --> (4 `sel` 1)
--                       , (2 `except` [5])     --> (3 `sel'` [1..10])
--                       , (1 `sel` 6)          <-- (3 `sel` 1)
--                       , (1 `sel` 7)          <-- (3 `sel` 2)
--                       ]

