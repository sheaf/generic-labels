{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -O1 #-}

module ShouldCompile.Inject.Basic where

-- inspection-testing
import Test.Inspection
  ( inspectTest, (===) )
import qualified Test.Inspection as Inspection
  ( Result(..) )

-- generic-labels
import Data.Label
  ( (:=)(..) )
import Data.Generic.Labels
  ( Inject(..) )

--------------------------------------------------------------------------------

test, manual
  :: ( "a" := Double, "b" := Float )
  -> ( "b" := Float, "c" := Int, "a" := Double )
  -> ( "b" := Float, "c" := Int, "a" := Double )

test = inject

manual ( double, float ) ( _, int, _ ) = ( float, int, double )

result :: Inspection.Result
result = $( inspectTest $ 'manual === 'test )
