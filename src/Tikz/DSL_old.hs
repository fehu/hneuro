-----------------------------------------------------------------------------
--
-- Module      :  Tikz.DSL_old
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


{-# LANGUAGE ExistentialQuantification, TypeSynonymInstances, FlexibleInstances #-}


module Tikz.DSL_old (

  Tikz(..)
, TikzElem(..)
, TikzAttr(..)

, SomeTikzElem(..)
, SomeTikzAttr(..)

, TikzExpression(..)
, style
, foreach

, TikzNode

) where


import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as List

--import qualified Data.ByteString.Char8 as BS8
--import qualified Data.ByteString.UTF8 as UTF


-----------------------------------------------------------------------------

data Tikz = Tikz ByteString

class TikzElem a where tikz2str   :: a -> Int -> ByteString
                       tikzAppend :: (TikzElem b) => a -> b -> Int -> ByteString

                       tikzAppend x y i = tikz2str x i `BS.append` tikz2str y i

class (TikzElem a) => TikzAttr a


data SomeTikzElem = forall a . (TikzElem a) => TikzElem a
data SomeTikzAttr = forall a . (TikzAttr a) => TikzAttr a

instance TikzElem SomeTikzElem where tikz2str (TikzElem x) = tikz2str x
instance TikzElem SomeTikzAttr where tikz2str (TikzAttr x) = tikz2str x
instance TikzAttr SomeTikzAttr

instance TikzElem String where tikz2str s _ = BS.pack s
instance TikzAttr String

-----------------------------------------------------------------------------

data TikzExpressionType = TikzExpressionCmd
--                        | TikzExpressionCmdBlock
                        | TikzExpressionEnv

data TikzExpression = TikzExpression { tikzExprName      :: String
                                     , tikzExprMainArg   :: Maybe SomeTikzAttr
                                     , tikzExprExtraArgs :: [SomeTikzAttr]
--                                     , tikzExprType      :: TikzExpressionType
                                     , tikzExprBody      :: [SomeTikzElem]
                                     }
                 | RawTikzCommand { tikzExprRaw  :: ByteString
--                                  , tikzExprType :: TikzExpressionType
                                  , tikzExprBody :: [SomeTikzElem]
                                  }

-----------------------------------------------------------------------------

nextInd = (+) 4
mkBS = BS.concat . map BS.pack

mkBody [TikzElem a] ind = undefined -- TODO

--mkBody [TikzElem a] ind = BS.concat $ mkBS ["\n", replicate ind ' ']
--                                    : [tikz2str a $ nextInd ind]

--mkBody ((TikzElem a) : t) ind = undefined -- TODO

mkMainArg (Just a) = BS.concat [ BS.pack "{"
                               , tikz2str a 0
                               , BS.pack "}"
                               ]
mkMainArg _        = BS.pack ""

mkExtraArgs [] = BS.pack ""
mkExtraArgs as = BS.concat [ BS.pack "["
                           , BS.intercalate (BS.pack ",") $ map (`tikz2str` 0) as
                           , BS.pack "]"]

-----------------------------------------------------------------------------

instance TikzElem TikzExpression where
    tikz2str (TikzExpression name mArg args body) ind = cmd
        where i = replicate ind ' '
              cmd = BS.append (mkBS ["\n", i, "\\", name])
                              (BS.concat [ mkMainArg mArg
                                         , mkExtraArgs args
                                         , mkBody body $ nextInd ind
                                         ]
                              )

    tikz2str (RawTikzCommand raw body) ind = raw `BS.append` mkBody body (nextInd ind)

style :: String -> [SomeTikzAttr] -> TikzExpression
style name attrs = TikzExpression "style"
                                   (Just $ TikzAttr name)
                                   []
--                                   TikzExpressionCmd
                                   [TikzElem $ RawTikzCommand style []]
                    where style = BS.intercalate (BS.pack "") $ map (`tikz2str` 0) attrs


foreach :: [String] -> String -> ([String] -> [SomeTikzElem]) -> TikzExpression
foreach args in' mkBody = RawTikzCommand (BS.pack fbody) (mkBody args)
                    where fbody = List.unwords ["foreach", vars, "in", in']
                          vars  = List.intercalate " / " $ fmap ('\\' :) args


picture :: [SomeTikzAttr] -> [SomeTikzElem] -> TikzExpression
picture = TikzExpression "begin" (Just $ TikzAttr "tikzpicture")

-----------------------------------------------------------------------------

-- \node[input neuron, pin=left:Input \#\y] (I-\name) at (0,-\y) {};
data TikzNode = TikzNode { tikzNodeName :: String
                         , tikzNodeArgs ::[SomeTikzAttr]
                         , tikzNodeAt   :: Maybe String
                         , tikzNodeBody :: [SomeTikzElem]
                         }

instance TikzElem TikzNode where
    tikz2str (TikzNode name args at body) ind = BS.concat [
        BS.pack "\\node"
      , mkExtraArgs args
      , BS.pack $ "(" ++ name ++ ")"
      , case at of Just a -> BS.pack $ "at (" ++ a ++ ")"
                   _      -> BS.empty
      , BS.pack " "
      , case body of []  -> BS.pack "{}"
                     [x] -> BS.concat [BS.pack "{", tikz2str x 0 , BS.pack "}"]
                     _   -> BS.concat [ BS.pack "{\n"
                                      , BS.concat $ map (`tikz2str` nextInd ind) body
                                      , BS.pack "\n}\n"
                                      ]
      , BS.pack ";"
      ]


-----------------------------------------------------------------------------


