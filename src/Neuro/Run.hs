module Neuro.Run(
  NNet
, NetworkInput
, NeuroF
, ExecutionAccumulator
, nnetIter
) where

import Data.Map ( Map, (!), assocs, empty, elems, filter, filterWithKey, fromList
                , insert,  keysSet, size, toList,  union, unionWith, member, mapWithKey )
--import Data.Map hiding ( map )

import NamedFunc

import Neuro

type MapLike k a = [(k, a)]

--      walk layer by layer -->
--                               calc layer outputs using previous layers
--                             * guard network output values
--      reached last layer  --|
--
--      walk layer by layer <--
--                              gather layers back


type NNet a = [Layer a]

type NetworkInput a = Map ElemId a

--type IsSnapshot = Bool
type NeuroF a = Maybe (Neuron a -> Neuron a)

type ExecutionAccumulator a = Map ElemId a

nnetIter :: (Num a, Show a) => SynapsesCache
                            -> NetworkInput a
                            -> NNet a
                            -> ExecutionAccumulator a
                            -> NeuroF a
                            -> (NNet a, ExecutionAccumulator a)

--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --
--  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  -- --  --


processElem  :: NeuroF a
             -> SynapsesCache
             -> ExecutionAccumulator a
             -> NetworkElem a
             -> (NetworkElem a, ExecutionAccumulator a)
processLayer :: SynapsesCache
             -> ExecutionAccumulator a
             -> Layer a
             -> NeuroF a
             -> (Layer a, ExecutionAccumulator a)


setInput :: NetworkElem a -> a -> NetworkElem a
setInput i v = updateInput i (\_ -> v)

addDelayedInput :: NetworkElem a -> a -> NetworkElem a
addDelayedInput i v = updateDelayedInput i (\vs -> v:vs)

setOutput :: NetworkElem a -> a -> NetworkElem a
setOutput i v = updateOutput i (\_ -> v)

processNeuron syns acc nEl mbF = (newElem, newAcc)
                             where newAcc = insert id (napply f l) acc
                                   l = [ napply wf x (acc ! i) | (x, i) <- zip w $ getTo syns id ]
                                   f  = getTf nEl
                                   wf = getWf nEl
                                   w  = getWs nEl
                                   id = getId nEl
                                   newElem = maybe nEl ($ nEl) mbF

processInput syns acc nEl _ = (newElem, acc)
                             where newElem = setInput nEl $ acc ! getId nEl

processDelayedInput syns acc nEl _ = (newElem, acc)
                             where newElem = addDelayedInput nEl $ acc ! getId nEl

processOutput syns acc nEl _ = (newElem, acc)
                             where newElem = setOutput nEl $ acc ! getId nEl

processElem mbF syns acc nEl | isNeuron nEl       = processNeuron       syns acc nEl mbF
                             | isInput nEl        = processInput        syns acc nEl mbF
                             | isDelayedInput nEl = processDelayedInput syns acc nEl mbF -- TODO
                             | isOutput nEl       = processOutput       syns acc nEl mbF


processLayer syns acc layer mbF = (newEl, snd fres)
                                where fres = foldr f ([], acc) $ elems layer
                                      f x (els, a) = (el:els, rAcc)
                                                   where res  = processElem mbF syns a x
                                                         el   = fst res
                                                         rAcc = snd res
                                      newEl = fromList $ map (\e -> (getId e, e)) $ fst fres

nnetIterInner :: Num a => SynapsesCache
                       -> NNet a
                       -> ExecutionAccumulator a
                       -> NeuroF a
                       -> (NNet a, ExecutionAccumulator a)
nnetIterInner syns (x:[]) acc mbF = ([fst res], snd res)
                                  where res = processLayer syns acc x mbF
nnetIterInner syns (x:xs) acc mbF = case rec of (rxs, racc) -> ((fst lRes):rxs, racc)
                                           where rec = nnetIterInner syns xs (snd lRes) mbF
                                                 lRes = processLayer syns acc x mbF

-- 1. if acc is not empty, copy the values connected to delayed inputs and clear the rest
-- 2. add inouts to the acc
-- 3. run the inner step
nnetIter syns inp nnet acc mbF = nnetIterInner syns nnet nAcc mbF
                               where nAcc = union inp $ fromList $ foldr (++) [] $ map f $ assocs cleanAcc
                                     f (k, x) = map (\kk -> (kk, x)) $ backloops syns ! k
                                     cleanAcc = filterWithKey (\k _ -> member k $ backloops syns) acc

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
