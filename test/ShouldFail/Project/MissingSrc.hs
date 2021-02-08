{-# LANGUAGE OverloadedLabels #-}

module ShouldFail.Project.MissingSrc where

-- generic-labels
import Data.Label
  ( (:=)(..) )
import Data.Generic.Labels
  ( Project(..) )

--------------------------------------------------------------------------------

test :: ( "b" := Float, "c" := Int, "a" := Bool ) -> ( "a" := Bool, "b" := Float, "d" := Int )
test = project
