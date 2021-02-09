{-# LANGUAGE OverloadedLabels #-}

module Example where

-- generic-labels
import Data.Generic.Labels

--------------------------------------------------------------------------------

type AllArgs = ( "f1" := Float, "f2" := Float, "f3" := Float, "f4" := Float, "b" := Bool )
type OptArgs = ( "f3" := Float, "f4" := Float, "b" := Bool )

foo :: AllArgs -> res
foo = undefined

adaptedFoo :: Adapt args OptArgs AllArgs => args -> res
adaptedFoo args = foo ( adapt args defaults )
  where
    defaults :: OptArgs
    defaults = ( #f3 := 0, #f4 := 1, #b := False )


call1, call2, call3 :: res
call1 = adaptedFoo ( #f1 := 0.5, #f2 := 1.0 )
call2 = adaptedFoo ( #f2 := 1.0, #b := True, #f1 := 0.5 )
call3 = adaptedFoo ( #f1 := 0.5, #f2 := 1.0, #f3 := -1.0, #f4 := 2.0 )

call4 :: res
call4 = adaptedFoo ( #f1 := 0.5, #b := True )

call5 :: res
call5 = adaptedFoo ( #f1 := 0.5, #f2 := 1.0, #xxx := "redundant" )
