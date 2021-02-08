{-# LANGUAGE OverloadedLabels #-}

module ShouldFail.Project.LabelMismatchPoly where

-- generic-labels
import Data.Label
  ( (:=)(..) )
import Data.Generic.Labels
  ( Project(..) )

--------------------------------------------------------------------------------

test :: ( "b" := Int, "a" := Int )
test = project ( #a := 17.7, #b := 9 )
