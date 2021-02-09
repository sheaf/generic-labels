{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module ShouldFail.Project.PolymorphicR where

-- base
import GHC.Generics
  ( Generic )

-- generic-labels
import Data.Generic.Labels
  ( project )

-------------------------------------------------------------------

default ()

data AB a b = AB { a :: a, b :: b } deriving stock Generic

newtype B b = B { b :: b } deriving stock Generic

test :: Num b => B b
test = project ( AB { a = 0.5, b = 7 } )
