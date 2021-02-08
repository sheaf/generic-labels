{-# LANGUAGE OverloadedLabels #-}

module ShouldFail.Project.Unlabelled where

-- generic-labels
import Data.Generic.Labels
  ( Project(..) )

--------------------------------------------------------------------------------

test :: ( Float, Int ) -> ( Int, Float )
test = project
