module Neuro.Run
(

) where

import Neuro

--      walk layer by layer -->
--                               calc layer outputs using previous layers
--                             * guard network output values
--      reached last layer  --|
--
--      walk layer by layer <--
--                              gather layers back

-- ?? --                             put guarded * outputs into corresponding inputs
-- ?? --    return modified network


data NetworkState a  = NS [Layer a]

--nnetIter :: ExecutionAccumulator a -> NetworkState a -> (NetworkState a, ExecutionAccumulator a)
nnetIter :: NetworkState a -> NetworkState a

--processLayerReverse :: ExecutionAccumulator a -> Layer a -> (Layer a, ExecutionAccumulator a)

data ExecutionAccumulator a = Acc [(ElemId, a)]

processLayer :: ExecutionAccumulator a -> Layer a -> ExecutionAccumulator a




processLayer acc layer = acc -- TODO

nnetIterInner :: (NetworkState a, ExecutionAccumulator a) -> (NetworkState a, ExecutionAccumulator a)
nnetIterInner (NS(x:[]), acc) = (NS [x], processLayer acc x)
nnetIterInner (NS(x:xs), acc) = case rec of (NS rxs, racc) -> (NS (x:rxs), racc)
                              where rec = nnetIterInner ((NS xs), (processLayer acc x))

nnetIter x = fst $ nnetIterInner (x, Acc [])
