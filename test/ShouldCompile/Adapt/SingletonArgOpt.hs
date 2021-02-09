{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}

module ShouldCompile.Adapt.SingletonArgOpt where

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

test, manual :: ( "a" := Int, "b" := Bool )

test = adapt ( #a := 3 ) ( #b := True )

manual = ( #a := 3, #b := True )

result :: Inspection.Result
result = $( inspectTest $ 'manual === 'test )
