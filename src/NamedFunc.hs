module NamedFunc( NamedFunc, named )
where

data NamedFunc f = NamedFunc String f

named :: f -> String -> NamedFunc f
f `named` s = NamedFunc s f

instance Show (NamedFunc f) where show (NamedFunc n _) = n

instance Eq (NamedFunc f)   where (NamedFunc n1 _) == (NamedFunc n2 _) = n1 == n2
