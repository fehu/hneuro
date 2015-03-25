module NamedFunc( NamedFunc, named )
where

data NamedFunc f = NamedFunc String f

named :: f -> String -> NamedFunc f
f `named` s = NamedFunc s f

instance Show (NamedFunc f) where show (NamedFunc n _) = n
