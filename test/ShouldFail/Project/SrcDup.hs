{-# LANGUAGE OverloadedLabels #-}

module ShouldFail.Project.SrcDup where

-- generic-labels
import Data.Label
  ( (:=)(..) )
import Data.Generic.Labels
  ( Project(..) )

--------------------------------------------------------------------------------

test :: ( "a" := Float, "a" := Float, "b" := Int ) -> ( "b" := Int, "a" := Float )
test = project
