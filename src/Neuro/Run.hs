module Neuro.Run
(

) where

import Data.Map ( Map, (!), insert, elems, fromList, empty, filterWithKey )
--import Data.Map hiding ( map )

import NamedFunc

import Neuro

--      walk layer by layer -->
--                               calc layer outputs using previous layers
--                             * guard network output values
--      reached last layer  --|
--
--      walk layer by layer <--
--                              gather layers back


data NetworkState a  = NS [Layer a]

type NetworkInput a = [a]

--nnetIter :: Num a => SynapsesCache -> NetworkInput a -> NetworkState a -> ExecutionAccumulator a
--                                                     -> (NetworkState a, ExecutionAccumulator a)


type ExecutionAccumulator a = Map ElemId a
-- SynapsesCache
processElem  :: SynapsesCache -> ExecutionAccumulator a -> NetworkElem a -> ExecutionAccumulator a
processLayer :: SynapsesCache -> ExecutionAccumulator a -> Layer a -> ExecutionAccumulator a








processElem syns acc n = insert id (napply f l) acc
                       where l = [ napply wf x (acc ! i) | (x, i) <- zip w $ getTo syns id ]
                             f  = getTf n
                             wf = getWf n
                             w  = getWs n
                             id = getId n

processLayer syns acc layer = foldr (\x a -> processElem syns a x) acc $ elems layer

nnetIterInner :: Num a => (SynapsesCache, NetworkState a, ExecutionAccumulator a)
                        -> (NetworkState a, ExecutionAccumulator a)
nnetIterInner (syns, NS(x:[]), acc) = (NS [x], processLayer syns acc x)
nnetIterInner (syns, NS(x:xs), acc) = case rec of (NS rxs, racc) -> (NS (x:rxs), racc)
                              where rec = nnetIterInner (syns, (NS xs), (processLayer syns acc x))

--nnetIter syns inp st acc = nnetIterInner (syns, st, newAcc)
--                         where newAcc = filterWithKey (\k _ -> isOutput $ get k) acc


--firstNnetIter syns st = nnetIter syns st
