{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -O1 #-}

module ShouldCompile.Project.PolymorphicT where

-- inspection-testing
import Test.Inspection
  ( inspectTest, (===) )
import qualified Test.Inspection as Inspection
  ( Result(..) )

-- generic-labels
import Data.Label
  ( (:=)(..) )
import Data.Generic.Labels
  ( Project(..) )

--------------------------------------------------------------------------------

default ()

test, manual :: ( "b" := Int, "c" := Char, "a" := Float )

test = project ( #c := 'c', #a := 17.7, #b := 9 )

manual = ( #b := 9, #c := 'c', #a := 17.7 )

result :: Inspection.Result
result = $( inspectTest $ 'manual === 'test )
