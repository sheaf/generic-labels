{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module ShouldCompile.Inject.PolymorphicR where

-- base
import GHC.Generics

-- generic-labels
import Data.Generic.Labels
  ( Inject(..) )

--------------------------------------------------------------------------------

default ()

data AB a b = AB { a :: a, b :: b }
  deriving stock Generic

data CAB c a b = CAB { c :: c, a :: a, b :: b }
  deriving stock Generic


test, manual :: RealFrac b => CAB Float Int b -> CAB Float Int b

test = inject ( AB { a = 3, b = 7.7 } )

manual = \ ( CAB { c } ) -> CAB { c, a = 3, b = 7.7 }
