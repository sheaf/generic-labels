{-# LANGUAGE OverloadedLabels #-}

module ShouldFail.Adapt.SuperflousArgs where

-- generic-labels
import Data.Label
  ( (:=)(..) )
import Data.Generic.Labels
  ( Adapt(..) )

--------------------------------------------------------------------------------

test :: ( "i" := Int, "f" := Float, "c" := Char ) -> ( "f" := Float, "b" := Bool, "i" := Int )
test args = adapt args ( #b := False )
