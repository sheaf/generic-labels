{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}


module ShouldFail.Adapt.Unlabelled where

-- base
import GHC.Generics

-- generic-labels
import Data.Label
  ( (:=)(..) )
import Data.Generic.Labels
  ( Adapt(..) )

--------------------------------------------------------------------------------

data FB = FB
  { float :: Float
  , int   :: Int
  }
  deriving stock Generic

test :: ( "int" := Int, "float" := Float ) -> ( "bool" := Bool, FB )
test args = adapt args ( #bool := False )
