{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -O1 #-}

module ShouldCompile.Project.PolymorphicR where

-- base
import GHC.Generics
  ( Generic )

-- inspection-testing
import Test.Inspection
  ( inspectTest, (===) )
import qualified Test.Inspection as Inspection
  ( Result(..) )

-- generic-labels
import Data.Generic.Labels
  ( project )

-------------------------------------------------------------------

default ()

data AB b = AB { a :: Float, b :: b } deriving stock Generic

newtype B b = B { b :: b } deriving stock Generic

test, manual :: Num b => B b
test = project ( AB { a = 0.5, b = 7 } )

manual = B { b = 7 }

result :: Inspection.Result
result = $( inspectTest $ 'manual === 'test )
