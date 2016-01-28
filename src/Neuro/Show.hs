-----------------------------------------------------------------------------
--
-- Module      :  Neuro.Show
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Neuro.Show (

  NShow(..)

) where

class NShow net format where nShow :: net -> format

