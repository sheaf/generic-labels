{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Type.List
  ( (:++:), Elem, Intersect, Remove )
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

type Elem :: k -> [k] -> Bool
type family Elem x xs where
  Elem _ '[]         = False
  Elem x ( x ': _ )  = True
  Elem x ( _ ': ys ) = Elem x ys

type Remove :: k -> [k] -> ( [k], Mult )
type family Remove x ys where
  Remove _ '[] = '( '[], None )
  Remove x ( x ': ys ) = RemoveHelper Nothing    ( Remove x ys )
  Remove x ( y ': ys ) = RemoveHelper ( Just y ) ( Remove x ys )

type RemoveHelper :: Maybe k -> ( [k], Mult ) -> ( [k], Mult )
type family RemoveHelper new rest where
  RemoveHelper Nothing    '( rest, m ) = '(      rest, m `AddMult` One )
  RemoveHelper ( Just y ) '( rest, m ) = '( y ': rest, m )

-- | Intersect two lists, removing duplicates.
type Intersect :: [k] -> [k] -> [k]
type family Intersect as bs where
  Intersect '[] _ = '[]
  Intersect _ '[] = '[]
  Intersect ( a ': as ) bs = IntersectHelper a as ( Remove a bs )

type IntersectHelper :: k -> [k] -> ( [k], Mult ) -> [k]
type family IntersectHelper a as rem_a_bs where
  IntersectHelper _ as '( bs, None ) =      Intersect as bs
  IntersectHelper a as '( bs, _    ) = a ': Intersect as bs
