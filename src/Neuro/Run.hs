module Neuro.Run
(

) where

import Data.Map ( Map, (!), assocs, empty, elems, filter, filterWithKey, fromList
                , insert,  keysSet, size, toList,  union, unionWith )
--import Data.Map hiding ( map )

import NamedFunc

import Neuro

ensuring cond err a = if cond then a
                              else error err

tupled :: (a -> b -> c) -> (a, b) -> c
tupled f (x, y) = f x y

type MapLike k a = [(k, a)]

zipByKey :: (Eq k, Ord k) => Map k a -> Map k b -> MapLike k (a, b)
zipByKey m1 m2 = ensuring (keysSet m1 == keysSet m2) "Maps have different keys" $
                    map (\(k, v1) -> (k, (v1, m2 ! k))) $ assocs m1

--mapValues :: (a -> b) -> Map k a -> Map k b
--mapValues f m = Data.Map.map f m

-- unionWith (\(a, b) -> ) m1 m2

--      walk layer by layer -->
--                               calc layer outputs using previous layers
--                             * guard network output values
--      reached last layer  --|
--
--      walk layer by layer <--
--                              gather layers back


type NetworkState a  = [Layer a]

type NetworkInput a = Map ElemId a

--nnetIter :: (Num a, Show a) => SynapsesCache -> NetworkInput a -> NetworkState a -> ExecutionAccumulator a TODO
--                                                     -> (NetworkState a, ExecutionAccumulator a)


type ExecutionAccumulator a = Map ElemId a
-- SynapsesCache
processElem  :: SynapsesCache -> ExecutionAccumulator a -> NetworkElem a -> ExecutionAccumulator a
processLayer :: SynapsesCache -> ExecutionAccumulator a -> Layer a -> ExecutionAccumulator a


setInput :: NetworkElem a -> a -> NetworkElem a
setInput i v = updateInput i (\_ -> v)




processElem syns acc n = insert id (napply f l) acc
                       where l = [ napply wf x (acc ! i) | (x, i) <- zip w $ getTo syns id ]
                             f  = getTf n
                             wf = getWf n
                             w  = getWs n
                             id = getId n

processLayer syns acc layer = foldr (\x a -> processElem syns a x) acc $ elems layer

nnetIterInner :: Num a => (SynapsesCache, NetworkState a, ExecutionAccumulator a)
                        -> (NetworkState a, ExecutionAccumulator a)
nnetIterInner (syns, (x:[]), acc) = ([x], processLayer syns acc x)
nnetIterInner (syns, (x:xs), acc) = case rec of (rxs, racc) -> (x:rxs, racc)
                              where rec = nnetIterInner (syns, xs, (processLayer syns acc x))

--nnetIter syns inp st@(x:xs) acc = nnetIterInner (syns, st, fromAcc) -- union fromAcc inpAcc TODO
--                                where f = \k _ -> idType k == NetO
--                                      fromAcc = filterWithKey f acc -- TODO
--
--                                      inpAcc  = if size inputs == size inp
--                                                then map (\(k, (e, v)) -> (k, setInput e v)) $
--                                                    zipByKey inputs inp
----                                                then mapValues (\(a, b) -> setInput a b) $ zipByKey inputs inp -- tupled
--                                                else error ("input of wrong size " ++ show inp)
----                                      fset (e, v) =
--                                      inputs  = Data.Map.filter isInput x -- map snd $ toList $


--firstNnetIter syns st = nnetIter syns st
