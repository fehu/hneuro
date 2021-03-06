-----------------------------------------------------------------------------
--
-- Module      :  Tikz.DSL
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--

{-# LANGUAGE ExistentialQuantification, TypeSynonymInstances, FlexibleInstances #-}

module Tikz.DSL (

  Repr

, Elem(..)
, ElemType(..)
, TypeOfElem(..)

, Attr(..)

, Expr(..)
, ExprType(..)
, TypeOfExpr(..)

, Cmd(..)
, Env(..)

, EmptyLine(..)
, CommentLines(..)
, RawExpr(..)

, picture
, PictureDefs(..)
, PictureAttrs(..)
, PictureStyles(..)

, Style(..)
, Def(..)

, Node(..)

, Path(..)
, PathType(..)

) where


import qualified Data.List as List

import Prelude hiding (concat, unwords, lines)
import Data.ByteString.Char8 (ByteString, pack, concat, intercalate, unwords, append)
import qualified Data.ByteString.Char8 as BS

-----------------------------------------------------------------------------

type Repr = ByteString

type Indent = Int

-----------------------------------------------------------------------------


data ElemType = TAttr | TExpr
data ExprType =  TCmd | TEnv

type family TypeOfElem e :: ElemType
type family TypeOfExpr e :: ExprType

class Elem a where -- elemType      :: a -> ElemType
                   elemMultiline :: a -> Bool
                   elemRepr      :: Indent -> a -> ByteString

data Attr = forall e . (Elem e, TypeOfElem e ~ TAttr) => Attr e
data Expr = forall e . (Elem e, TypeOfElem e ~ TExpr) => Expr e

data Cmd = forall e . (Elem e, TypeOfElem e ~ TExpr, TypeOfExpr e ~ TCmd) => Cmd e
data Env = Env { envName  :: String
               , envAttrs :: [Attr]
               , envBody  :: [Expr]
               }
data EmptyLine = EmptyLine
data CommentLines = CommentLines [String]

data RawExpr = RawExpr String


instance Elem Attr where -- elemType _ = TAttr
                         elemMultiline (Attr a) = elemMultiline a
                         elemRepr i (Attr a) = elemRepr i a
instance Elem Expr where -- elemType _ = TExpr
                         elemMultiline (Expr e) = elemMultiline e
                         elemRepr i (Expr e) = elemRepr i e
instance Elem Cmd  where -- elemType _ = TExpr
                         elemMultiline (Cmd c)  = elemMultiline c
                         elemRepr i (Cmd c) = elemRepr i c

instance Elem Env  where -- elemType _ = TExpr
                         elemMultiline _ = True
                         elemRepr i e = concat [
                              indent i
                            , latexKey "begin"
                            , key
                            , optArgs $ envAttrs e
                            , newline
                            , lines . map (elemRepr (nextIndent i)) $ envBody e
                            , newline
                            , indent i
                            , latexKey "end"
                            , key
                            ]
                            where key = surroundBracketsCurly . pack $ envName e

instance Elem EmptyLine where -- elemType _ = TExpr
                              elemMultiline _ = True
                              elemRepr _ _ = newline

instance Elem CommentLines where -- elemType _ = TExpr
                                 elemMultiline _ = True
                                 elemRepr i (CommentLines ls) = pack $ pre
                                                                    ++ List.intercalate pre ls
                                                                    ++ "\n"
                                                              where pre = indent' i ++ "% "

instance Elem RawExpr where elemMultiline (RawExpr s) = '\n' `elem` s
                            elemRepr _ (RawExpr s) = pack s

type instance TypeOfElem Attr = TAttr
type instance TypeOfElem Expr = TExpr
type instance TypeOfElem Cmd  = TExpr
type instance TypeOfElem Env  = TExpr
type instance TypeOfElem EmptyLine    = TExpr
type instance TypeOfElem CommentLines = TExpr
type instance TypeOfElem RawExpr = TExpr

-----------------------------------------------------------------------------

newtype PictureDefs   = PictureDefs [Def]
newtype PictureAttrs  = PictureAttrs [Attr]
newtype PictureStyles = PictureStyles [Style]

-----------------------------------------------------------------------------

indent  = pack . indent'
indent' = flip replicate ' '

nextIndent = (+) 4

newline = pack "\n"

lines = intercalate (pack "\n")


latexKey k = pack $ "\\" ++ k

surround  pre suf x = concat [pack pre, x, pack suf]
surround' pre suf x = concat $ map pack [pre, x, suf]

surroundBracketsCurly  = surround "{" "}"
surroundBracketsSquare = surround "[" "]"
surroundBracketsRound  = surround "(" ")"

surroundBracketsCurly'  = surround' "{" "}"
surroundBracketsSquare' = surround' "[" "]"
surroundBracketsRound'  = surround' "(" ")"

surroundSpace s = surround s' s'
                where s' = replicate s ' '

optArgs [] = BS.empty
optArgs as = surroundBracketsSquare . intercalate (pack ",") $ map (elemRepr 0) as

-----------------------------------------------------------------------------

type instance TypeOfElem String = TAttr

instance Elem String where elemMultiline _ = False
                           elemRepr _ = pack

-----------------------------------------------------------------------------

picture (PictureAttrs attrs) (PictureStyles styles) (PictureDefs defs) body =
    Env "tikzpicture" attrs ( map Expr styles ++ Expr EmptyLine
                            : map Expr defs ++ Expr EmptyLine
                            : body)

-----------------------------------------------------------------------------

data Style = Style String [Attr]

instance Elem Style where elemMultiline _ = False
                          elemRepr i (Style name vals) = concat [
                              indent i
                            , latexKey "tikzstyle"
                            , surroundBracketsCurly' name
                            , pack " = "
                            , optArgs vals
                            , pack ";"
                            ]

type instance TypeOfElem Style = TExpr


-----------------------------------------------------------------------------

data Def = Def String String

instance Elem Def where elemMultiline _ = False
                        elemRepr i (Def name val) = concat [
                            indent i
                          , latexKey "def"
                          , latexKey name
                          , surroundBracketsCurly' val
                          ]

type instance TypeOfElem Def = TExpr


-----------------------------------------------------------------------------

data Node = Node String [Attr] (Maybe String) [Expr]

instance Elem Node where
    elemMultiline (Node _ _ _ []) = False
    elemMultiline _               = True
    elemRepr i (Node name attrs at body) = concat [
          indent i
        , latexKey "node"
        , optArgs attrs
        , surroundBracketsRound' name
        , at'
        , body' `append` pack ";"
        ]
        where at' = case at of Just a -> concat [
                                            pack " at "
                                          , surroundBracketsRound' a
                                          ]
                               _      -> BS.empty

              body' = case body of [] -> pack "{}"
                                   [x] | not $ elemMultiline x -> surroundBracketsCurly
                                                                . surroundSpace 1
                                                                $ elemRepr 0 x
                                   _  -> concat [
                                              pack "{"
                                            , newline
                                            , lines $ map (elemRepr (nextIndent i)) body
                                            , newline
                                            , indent i
                                            , pack "}"
                                            ]

type instance TypeOfElem Node = TExpr

-----------------------------------------------------------------------------

data PathType = Edge

data Path = Path PathType String String

instance Elem Path where elemMultiline _ = False
                         elemRepr i (Path Edge from to) = concat [
                              indent i
                            , latexKey "path "
                            , surroundBracketsRound' from
                            , pack " edge "
                            , surroundBracketsRound' to
                            , pack ";"
                            ]

type instance TypeOfElem Path = TExpr

-----------------------------------------------------------------------------


