{-# LANGUAGE UndecidableInstances #-}

module Data.Type.List
  ( (:++:), Remove )
  where

-- generic-labels
import Data.Type.Multiplicity
  ( AddMult
  , Mult
    ( None, One )
  )

--------------------------------------------------------------------------------

infixr 5 :++:
type (:++:) :: [k] -> [k] -> [k]
type family xs :++: ys where
  '[]         :++: ys = ys
  ( x ': xs ) :++: ys = x ': xs :++: ys

type Remove :: k -> [k] -> ( [k], Mult )
type family Remove x ys where
  Remove _ '[] = '( '[], None )
  Remove x ( x ': ys ) = RemoveHelper Nothing    ( Remove x ys )
  Remove x ( y ': ys ) = RemoveHelper ( Just y ) ( Remove x ys )

type RemoveHelper :: Maybe k -> ( [k], Mult ) -> ( [k], Mult )
type family RemoveHelper new rest where
  RemoveHelper Nothing    '( rest, m ) = '(      rest, m `AddMult` One )
  RemoveHelper ( Just y ) '( rest, m ) = '( y ': rest, m )
