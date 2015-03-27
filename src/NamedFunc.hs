module NamedFunc
( NamedFunc
, named
, name
, napply
) where

data NamedFunc  a b   = NamedFunc  String (a -> b)

named :: (a -> b) -> String -> NamedFunc a b
name  :: NamedFunc a b -> String
napply  :: NamedFunc a b -> a -> b

x `named` n = NamedFunc n x
name (NamedFunc n _) = n
napply  (NamedFunc _ f) = f

instance Show (NamedFunc a b)    where show (NamedFunc n _) = n

instance Eq   (NamedFunc a b)    where (NamedFunc n1 _) == (NamedFunc n2 _) = n1 == n2
