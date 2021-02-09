{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -O1 #-}

module ShouldCompile.Project.Basic where

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

test, manual :: ( "b" := Float, "c" := Int, "a" := Double ) -> ( "a" := Double, "b" := Float )

test = project

manual ( float, _, double ) = ( double, float )

result :: Inspection.Result
result = $( inspectTest $ 'manual === 'test )
