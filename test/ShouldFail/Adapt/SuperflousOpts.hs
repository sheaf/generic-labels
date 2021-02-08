{-# LANGUAGE OverloadedLabels #-}

module ShouldFail.Adapt.SuperflousOpts where

-- generic-labels
import Data.Label
  ( (:=)(..) )
import Data.Generic.Labels
  ( Adapt(..) )

--------------------------------------------------------------------------------

test :: ( "i" := Int, "f" := Float ) -> ( "f" := Float, "b" := Bool, "i" := Int )
test args = adapt args ( #b := False, #c := '?' )
