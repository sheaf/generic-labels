{-# LANGUAGE OverloadedLabels #-}

module ShouldFail.Project.DstDup where

-- generic-labels
import Data.Label
  ( (:=)(..) )
import Data.Generic.Labels
  ( Project(..) )

--------------------------------------------------------------------------------

test :: ( "a" := Int, "b" := Float ) -> ( "a" := Int, "b" := Float, "a" := Int )
test = project
