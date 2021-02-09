{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -O1 #-}

module ShouldCompile.Adapt.TTR where

-- base
import GHC.Generics
  ( Generic )

-- inspection-testing
import Test.Inspection
  ( inspectTest, (===) )
import qualified Test.Inspection as Inspection
  ( Result(..) )

-- generic-labels
import Data.Label
  ( (:=)(..) )
import Data.Generic.Labels
  ( Adapt(..) )

--------------------------------------------------------------------------------

data FBI = FBI
  { float :: Float
  , bool  :: Bool
  , int   :: Int
  }
  deriving stock Generic

test, manual :: ( "int" := Int, "float" := Float ) -> FBI

test args = adapt args ( #bool := False )

manual ( _ := int, _ := float ) = FBI { float, bool = False, int }

result :: Inspection.Result
result = $( inspectTest $ 'manual === 'test )
