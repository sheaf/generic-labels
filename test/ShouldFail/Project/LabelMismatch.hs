{-# LANGUAGE OverloadedLabels #-}

module ShouldFail.Project.LabelMismatchSimple where

-- generic-labels
import Data.Label
  ( (:=)(..) )
import Data.Generic.Labels
  ( Project(..) )

--------------------------------------------------------------------------------

test :: ( "b" := Float, "a" := Bool, "c" := Double ) -> ( "c" := Double, "b" := Int )
test = project
