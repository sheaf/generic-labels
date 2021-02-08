{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module ShouldCompile.Adapt.RRR where

-- base
import GHC.Generics
  ( Generic )

-- inspection-testing
import Test.Inspection
  ( inspectTest, (===) )
import qualified Test.Inspection as Inspection
  ( Result(..) )

-- generic-labels
import Data.Generic.Labels
  ( Adapt(..) )

--------------------------------------------------------------------------------

data IFC = IFC { int :: Int, float :: Float, char :: Char }
  deriving stock Generic

data CFBI = CFBI
  { char  :: Char
  , float :: Float
  , bool  :: Bool
  , int   :: Int
  }
  deriving stock Generic

data BC = BC { bool :: Bool, char :: Char }
  deriving stock Generic

test, manual :: IFC -> CFBI

test args = adapt args ( BC { bool = False, char = '?' } )

manual ( IFC { int, float, char } ) = CFBI { char, float, int, bool = False }

result :: Inspection.Result
result = $( inspectTest $ 'manual === 'test )
