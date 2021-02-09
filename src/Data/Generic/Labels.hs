{-# LANGUAGE UndecidableInstances #-}

{-|
Module: Data.Generic.Labels
Description: Convert between collections of fields using generics.

This module provides functionality for converting between different collections of fields.

== Projections

To project out certain fields of a record, use 'project'.

For instance:

@
data IBXD x = IBXD { i :: Int, b :: Bool, x :: x, d :: Double }
  deriving stock Generic
data XI x = XI { x :: c, i :: Int }
  deriving stock Generic
@

We can project out the two fields of interest from the first record type:

@
ibxd_to_xi :: IBXD x -> XI x
ibxd_to_xi = project
@

== Injections

Going the other way, we can use 'inject' to override fields of the larger record
with those from the smaller record:

@
xi_into_ibxd :: XI x -> IBXD x -> IBXD x
xi_into_ibxd = inject
@

== Adapters

'project' and 'inject' are two instances of the more general 'adapt' function,
which allows us to only specify the missing arguments in the above example.

@
xi_plus_bd_makes_ibxd :: XI x -> ( "b" := Bool, "d" := Double ) -> IBXD x
xi_plus_bd_makes_ibxd = adapt
@

In this situation, we are building up a record of type @IBXD x@ out of two parts.

More generally, `adapt` allows for fields in the first argument to override
fields in the second argument, which provides a convenient mechanism for
named optional arguments.

@
adapt :: _ => givenArgs -> optionalArgs -> allArgs
@

For instance, if we have a function @f@ which takes in several named arguments

@
type AllArgsTuple = ( "arg1" := Ty1, "arg2" := Ty2, "arg3" := Ty3, "arg4" := Ty4 ) 

f :: AllArgsTuple -> r
@

and we have default values for some of those arguments, e.g.

@
type DefaultArgsTuple = ( "arg2" := Ty2, "arg3" := Ty3 )

f_defaults :: DefaultArgsTuple
f_defaults = ( #arg2 := val2, #arg3 := val3 )
@

then we can create a corresponding function @f_defaulting@,
which allows user to only pass the remaining (required) arguments:

@
f_defaulting
  :: CheckedAdapt args DefaultArgsTuple AllArgsTuple => args -> r
f_defaulting args = adapt args f_defaults
@

-}

module Data.Generic.Labels
  (
  -- * Converting between collections of fields
    Adapt(..), Inject(..), Project(..)

  -- * Re-export of labelling functionality from "Data.Label"
  , (:=)(..)

  -- * Unchecked functions (can behave unpredictably).
  , UncheckedAdapt(..), UncheckedInject(..), UncheckedProject(..)
  )
  where

-- base
import GHC.Generics

-- generic-labels
import Data.Label
  ( (:=)(..) )
import Data.Generic.Labels.Internal
  ( GAdapt
    ( gAdapt )
  )
import Data.Generic.Labels.Internal.Errors
  ( CheckAdapt, CheckInject, CheckProject )

--------------------------------------------------------------------------------

class UncheckedAdapt args opt all where
  -- | Create an adapter, without extra type-level validation.
  --
  -- Unchecked uses (e.g. presence of duplicate fields)
  -- can throw abstruse compile-time error messages
  -- or produce unexpected results at runtime.
  --
  -- Prefer using 'adapt' whenever possible.
  uncheckedAdapt :: args -> opt -> all

instance {-# OVERLAPPING #-} UncheckedAdapt a a a where
  uncheckedAdapt = const
instance {-# OVERLAPPING #-} UncheckedAdapt a opt a where
  uncheckedAdapt = const
instance {-# OVERLAPPING #-} ( a ~ b )
       => UncheckedAdapt ( lbl := a ) opt ( lbl := b ) where
  uncheckedAdapt = const
instance {-# OVERLAPPING #-} ( a ~ b, o ~ b )
       => UncheckedAdapt ( lbl := a ) ( lbl := o ) ( lbl := b ) where
  uncheckedAdapt = const

instance {-# OVERLAPPING #-}
    ( Generic all
    , argFld ~ S1 ( MetaSel ( Just lbl1 ) NoSourceUnpackedness NoSourceStrictness DecidedLazy ) ( Rec0 a )
    , optFld ~ S1 ( MetaSel ( Just lbl2 ) NoSourceUnpackedness NoSourceStrictness DecidedLazy ) ( Rec0 o )
    , GAdapt argFld optFld ( Rep all )
    )
  => UncheckedAdapt ( lbl1 := a ) ( lbl2 := o ) all where
  uncheckedAdapt ( _ := arg ) ( _ := opt ) =
    to $ gAdapt ( M1 ( K1 arg ) :: argFld x ) ( M1 ( K1 opt ) :: optFld x )

instance
    ( Generic opt, Generic all
    , argFld ~ S1 ( MetaSel ( Just lbl ) NoSourceUnpackedness NoSourceStrictness DecidedLazy ) ( Rec0 a )
    , GAdapt argFld ( Rep opt ) ( Rep all )
    )
  => UncheckedAdapt ( lbl := a ) opt all where
  uncheckedAdapt ( _ := arg ) opt =
    to $ gAdapt ( M1 ( K1 arg ) :: argFld x ) ( from opt )

instance {-# OVERLAPPING #-}
    ( Generic args, Generic all
    , optFld ~ S1 ( MetaSel ( Just lbl ) NoSourceUnpackedness NoSourceStrictness DecidedLazy ) ( Rec0 o )
    , GAdapt ( Rep args ) optFld ( Rep all )
    )
  => UncheckedAdapt args ( lbl := o ) all where
  uncheckedAdapt args ( _ := opt ) =
    to $ gAdapt ( from args ) ( M1 ( K1 opt ) :: optFld x )

instance {-# OVERLAPPABLE #-}
    ( Generic args, Generic opt, Generic all
    , GAdapt ( Rep args ) ( Rep opt ) ( Rep all )
    )
  => UncheckedAdapt args opt all where
  uncheckedAdapt args opt =
    to $ gAdapt ( from args ) ( from opt )

class    ( UncheckedAdapt args opt all ) => Adapt args opt all where
  -- | Create an adapter, to inject a smaller type into a larger one,
  -- providing defaults for optional values.
  --
  -- @
  --   myAdapt :: ( "i" := Int, "f" := Float ) -> ( "f" := Float, "b" := Bool, "i" := Int )
  --   myAdapt args = adapt args ( #b := False )
  -- @
  --
  -- @
  --   > myAdapt ( #i := 3, #f := 17.1 )
  --   > ( #f = 17.1, #b = False, #i := 3 )
  -- @
  --
  -- Here @ myAdapt @ re-arranges the arguments into the result,
  -- passing in additional (default) values that are overriden
  -- when they occur in the arguments.
  --
  -- Includes custom validation, e.g. to disallow duplicate arguments.
  -- Use 'uncheckedAdapt' to disable this validation
  -- (you might get strange errors!).
  adapt
    :: args -- ^ Provided arguments
    -> opt  -- ^ Default values of optional arguments
    -> all  -- ^ Combination of provided arguments and non-overridden defaults

instance ( UncheckedAdapt args opt all, CheckAdapt args opt all )
      => Adapt args opt all where
  adapt = uncheckedAdapt

class    UncheckedAdapt small big big => UncheckedInject small big where
  -- | Inject a smaller type into a larger one, without extra type-level validation.
  --
  -- Unchecked uses (e.g. presence of duplicate fields)
  -- can throw abstruse compile-time error messages
  -- or produce unexpected results at runtime.
  --
  -- Prefer using 'inject' whenever possible.
  uncheckedInject :: small -> big -> big

instance UncheckedAdapt small big big => UncheckedInject small big where
  uncheckedInject = uncheckedAdapt

class    ( UncheckedInject small big ) => Inject small big where
  -- | Inject a smaller type into a larger one,
  -- overriding the fields in the larger type with those from the smaller type.
  --
  -- @
  --   myInject
  --     :: ( "i" := Int, "f" := Float )
  --     -> ( "f" := Float, "b" := Bool, "i" := Int )
  --     -> ( "f" := Float, "b" := Bool, "i" := Int )
  --   myInject = inject
  -- @
  --
  -- Here @ myInject @ overrides the fields of the second argument
  -- with those provided in the first argument.
  --
  -- @
  --   > myInject ( #i := 3, #f := 17.1 ) ( #f := 9.0, #b := False, #i := 22 )
  --   > ( #f := 17.1, #b := False, #i := 3 )
  -- @
  --
  -- Includes custom validation, e.g. to disallow duplicate arguments.
  -- Use 'uncheckedInject' to disable this validation
  -- (you might get strange errors!).
  inject :: small -> big -> big

instance ( UncheckedInject small big, CheckInject small big )
      => Inject small big where
  inject = uncheckedInject

class    UncheckedAdapt big big small => UncheckedProject big small where
  -- | Project a smaller type out from a larger one, without extra type-level validation.
  --
  -- Unchecked uses (e.g. presence of duplicate fields)
  -- can throw abstruse compile-time error messages
  -- or produce unexpected results at runtime.
  --
  -- Prefer using 'project' whenever possible.
  uncheckedProject :: big -> small

instance UncheckedAdapt big big small => UncheckedProject big small where
  uncheckedProject big = uncheckedAdapt big big

class    ( UncheckedProject big small ) => Project big small where
  -- | Project a smaller type out from a larger one, discarding the rest.
  --
  -- @
  --   myProject :: ( "f" := Float, "b" := Bool, "i" := Int ) -> ( "i" := Int, "f" := Float )
  --   myProject = project
  -- @
  --
  -- Here @ myProject @ projects out a sub-component of the whole type,
  -- in this case discarding the boolean while re-arranging the other fields.
  --
  -- @
  --   > myProject ( #f := 17.1, #b := False, #i := 3 )
  --   > ( #i := 3, #f := 17.1 )
  -- @
  --
  -- Includes custom validation, e.g. to disallow duplicate arguments.
  -- Use 'uncheckedProject' to disable this validation
  -- (you might get strange errors!).
  project :: big -> small

instance ( UncheckedProject big small, CheckProject big small )
      => Project big small where
  project = uncheckedProject
