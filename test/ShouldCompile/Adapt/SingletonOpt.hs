{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -O1 #-}

module ShouldCompile.Adapt.SingletonOpt where

-- inspection-testing
import Test.Inspection
  ( inspectTest, (===) )
import qualified Test.Inspection as Inspection
  ( Result(..) )

-- generic-labels
import Data.Label
  ( (:=)(..) )
import Data.Generic.Labels
  ( Adapt(..) )

--------------------------------------------------------------------------------

test, manual :: ( "a" := Int, "b" := Bool, "c" := Char )

test = adapt ( #a := 3, #b := True ) ( #c := 'c' )

manual = ( #a := 3, #b := True, #c := 'c' )

result :: Inspection.Result
result = $( inspectTest $ 'manual === 'test )
