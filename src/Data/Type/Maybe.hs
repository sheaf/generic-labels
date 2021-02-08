module Data.Type.Maybe
  ( CatMaybes )
  where

--------------------------------------------------------------------------------

type CatMaybes :: [ Maybe a ] -> [ a ]
type family CatMaybes mbs where
  CatMaybes '[] = '[]
  CatMaybes ( Nothing ': mbs ) = CatMaybes mbs
  CatMaybes ( Just a ': mbs )  = a ': CatMaybes mbs
