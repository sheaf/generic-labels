module Data.Type.Error
  ( ErrorIfAmbiguous )
  where

-- base
import Data.Kind
  ( Constraint )

--------------------------------------------------------------------------------

type ErrorIfAmbiguous :: k -> Constraint -> l -> l
type family ErrorIfAmbiguous break err a where
  ErrorIfAmbiguous Dummy _ _ = Dummy
  ErrorIfAmbiguous _     _ a = a

type Dummy :: k
data family Dummy
