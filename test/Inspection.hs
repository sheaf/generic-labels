{-# LANGUAGE TemplateHaskell #-}

module Inspection
  ( tests )
  where

-- cabal
import qualified Distribution.TestSuite as Cabal

-- inspection-testing
import qualified Test.Inspection as Inspection
  ( Result(..) )

-- generic-labels-test
import qualified ShouldCompile.Adapt.RRR as Adapt.RRR
import qualified ShouldCompile.Adapt.TTR as Adapt.TTR
import qualified ShouldCompile.Adapt.TTT as Adapt.TTT
import qualified ShouldCompile.Adapt.SingletonArg    as Adapt.SingletonArg
import qualified ShouldCompile.Adapt.SingletonArgOpt as Adapt.SingletonArgOpt
import qualified ShouldCompile.Adapt.SingletonOpt    as Adapt.SingletonOpt
import qualified ShouldCompile.Inject.Basic as Inject.Basic
import qualified ShouldCompile.Inject.PolymorphicR  as Inject.PolymorphicR 
import qualified ShouldCompile.Project.Basic as Project.Basic
import qualified ShouldCompile.Project.PolymorphicR as Project.PolymorphicR
import qualified ShouldCompile.Project.PolymorphicT as Project.PolymorphicT

--------------------------------------------------------------------------------
-- Inspection tests.

tests :: IO [ Cabal.Test ]
tests = pure $ map mkCabalTest inspectionResults

inspectionResults :: [ ( String, Inspection.Result ) ]
inspectionResults =
  [ ( "Adapt.RRR", Adapt.RRR.result )
  , ( "Adapt.TTR", Adapt.TTR.result )
  , ( "Adapt.TTT", Adapt.TTT.result )
  , ( "Adapt.SingletonArg"   , Adapt.SingletonArg.result    )
  , ( "Adapt.SingletonArgOpt", Adapt.SingletonArgOpt.result )
  , ( "Adapt.SingletonOpt"   , Adapt.SingletonOpt.result    )
  , ( "Inject.Basic", Inject.Basic.result )
  , ( "Inject.PolymorphicR", Inject.PolymorphicR.result )
  , ( "Project.Basic", Project.Basic.result )
  , ( "Project.PolymorphicR", Project.PolymorphicR.result )
  , ( "Project.PolymorphicT", Project.PolymorphicT.result )
  ]

--------------------------------------------------------------------------------
-- Converting to 'Cabal' test types.

mkCabalTest :: ( String, Inspection.Result ) -> Cabal.Test
mkCabalTest ( testName, inspectionResult ) =
  Cabal.Test $
    Cabal.TestInstance
      { Cabal.run  = pure . Cabal.Finished . cabalResult $ inspectionResult
      , Cabal.name = testName
      , Cabal.tags = []
      , Cabal.options   = []
      , Cabal.setOption = \ _ _ -> Left "Test does not have any options."
      }

cabalResult :: Inspection.Result -> Cabal.Result
cabalResult ( Inspection.Failure err ) = Cabal.Error err
cabalResult _                          = Cabal.Pass
