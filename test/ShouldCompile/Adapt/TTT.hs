{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}

module ShouldCompile.Adapt.TTT where

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

test, manual :: ( "int" := Int, "float" := Float ) -> ( "float" := Float, "bool" := Bool, "int" := Int )

test args = adapt args ( #bool := False )

manual ( int, float ) = ( float, #bool := False, int )

result :: Inspection.Result
result = $( inspectTest $ 'manual === 'test )
