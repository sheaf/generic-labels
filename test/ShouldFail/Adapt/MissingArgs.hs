{-# LANGUAGE OverloadedLabels #-}

module ShouldFail.MissingArgs where

-- generic-labels
import Data.Generic.Labels
  ( Adapt(..) )

--------------------------------------------------------------------------------

test :: ( Int, Char, Float ) -> ( Float, Bool, Char, Int )
test args = adapt args ( False, '?' )
