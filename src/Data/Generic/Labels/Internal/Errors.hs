{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

{-|
Module: Data.Generic.Labels.Internal.Errors

Internal module providing custom type errors for invalid uses of
'Data.Generic.Labels.Adapt', 'Data.Generic.Labels.inject', 'Data.Generic.Labels.project'.

Consider for instance trying to project a source record onto a smaller target record,
but the source record is missing one of the fields:

@
missingField :: ( "a" := Bool, "c" := Double ) -> ( "c" := Double, "b" := Int )
missingField = project
@

Note that the source record is missing the @ "b" := Int @ field which is present in the target.

This results in the following error message:

@
  * No instance for
        Project
          ("b" := Float, "a" := Bool, "c" := Double)
          ("c" := Double, "b" := Int)
    The type being projected down is missing the following fields:
      - #b := Int
@
-}

module Data.Generic.Labels.Internal.Errors
  ( AdaptLabelMessage
  , CheckAdapt, CheckInject, CheckProject
  )
  where

-- base
import Data.Kind
  ( Constraint, Type )
import GHC.Generics
import GHC.TypeLits
  ( Symbol
  , TypeError, ErrorMessage(..)
  )

-- generic-lens-core
import Data.Generics.Product.Internal.GLens
  ( Eval, TyFun )

-- generic-labels
import Data.Label
  ( (:=) )
import Data.Type.Error
  ( ErrorIfAmbiguous, MessageIfNonEmpty, ThrowMessagesWithHeader )
import Data.Type.List
  ( (:++:), Intersect, Remove )
import Data.Type.Maybe
  ( CatMaybes )
import Data.Type.Multiplicity
  ( Mult ( None, One, Many ) )
import {-# SOURCE #-} Data.Generic.Labels
  ( Adapt(..), Inject(..), Project(..)
  , UncheckedAdapt(..), UncheckedInject(..), UncheckedProject(..)
  )

--------------------------------------------------------------------------------
-- Helper type families for improved error messages.

-- | Throw an error message when encountering two distinct types with the same label.
type AdaptLabelMessage :: Symbol -> Maybe Type -> Maybe Type -> Type -> Constraint
type family AdaptLabelMessage lbl mb_argTy mb_optTy allTy where
  AdaptLabelMessage _   Nothing     Nothing     ty = ( () :: Constraint )
  AdaptLabelMessage _   ( Just ty ) Nothing     ty = ( () :: Constraint )
  AdaptLabelMessage _   Nothing     ( Just ty ) ty = ( () :: Constraint )
  AdaptLabelMessage _   ( Just ty ) ( Just ty ) ty = ( () :: Constraint )
  AdaptLabelMessage lbl ( Just a )  Nothing     b  =
    TypeError
      (    Text "Mismatched types at label #" :<>: Text lbl :<>: Text "."
      :$$: Text "   Expected type: " :<>: ShowType b
      :$$: Text "   Provided type: " :<>: ShowType a
      )
  AdaptLabelMessage lbl Nothing    ( Just o )   b =
    TypeError
      (    Text "Mismatched types at label #" :<>: Text lbl :<>: Text "."
      :$$: Text "   Expected type: " :<>: ShowType b
      :$$: Text "   Optional type: " :<>: ShowType o
      )
  AdaptLabelMessage lbl ( Just a ) ( Just o ) b  =
    TypeError
      (    Text "Mismatched types at label #" :<>: Text lbl :<>: Text "."
      :$$: Text "   Expected type: " :<>: ShowType b
      :$$: Text "   Provided type: " :<>: ShowType a
      :$$: Text "   Optional type: " :<>: ShowType o
      )

-- | Throw an error message when an invalid use of 'Data.Generic.Labels.Adapt' is encountered:
--   - a field of the destination is missing in the source,
--   - a field that appears in both the source and destination appears more than once in either,
--   - a 'Generic' instance is missing.
type CheckAdapt :: Type -> Type -> Type -> Constraint
type family CheckAdapt args opt all where
  CheckAdapt a a a = ( () :: Constraint )
  CheckAdapt a opt a = ( () :: Constraint )
  CheckAdapt ( lbl := a ) ( lbl := o ) ( lbl := b ) =
    ( AdaptLabelMessage lbl ( Just a ) ( Just o ) b, a ~ b, o ~ b )
  CheckAdapt ( lbl := a ) opt ( lbl := b ) =
    ( AdaptLabelMessage lbl ( Just a ) Nothing b, a ~ b )
  CheckAdapt args ( lbl := opt ) all =
    ( ProperAdapt args opt all
        ( CollectLeaves ( Rep args ) )
        ( CollectLeaves ( S1 ( MetaSel ( Just lbl ) NoSourceUnpackedness NoSourceStrictness DecidedLazy ) ( Rec0 opt ) ) )
        ( CollectLeaves ( Rep all ) )
    , ErrorIfAmbiguous ( Rep args )
        ( TypeError
          (    Text "No instance for " :<>: ShowType ( Generic args )
          :$$: Text "arising from the constraint " :<>: ShowType ( Adapt args opt all )
          )
        )
        ( () :: Constraint )
    , ErrorIfAmbiguous ( Rep all )
        ( TypeError
          (    Text "No instance for " :<>: ShowType ( Generic all )
          :$$: Text "arising from the constraint " :<>: ShowType ( Adapt args opt all )
          )
        )
        ( () :: Constraint )
    )

  CheckAdapt args opt all =
    ( ProperAdapt args opt all
        ( CollectLeaves ( Rep args ) ) ( CollectLeaves ( Rep opt ) ) ( CollectLeaves ( Rep all ) )
    , ErrorIfAmbiguous ( Rep args )
        ( TypeError
          (    Text "No instance for " :<>: ShowType ( Generic args )
          :$$: Text "arising from the constraint " :<>: ShowType ( Adapt args opt all )
          )
        )
        ( () :: Constraint )
    , ErrorIfAmbiguous ( Rep opt )
        ( TypeError
          (    Text "No instance for " :<>: ShowType ( Generic opt )
          :$$: Text "arising from the constraint " :<>: ShowType ( Adapt args opt all )
          )
        )
        ( () :: Constraint )
    , ErrorIfAmbiguous ( Rep all )
        ( TypeError
          (    Text "No instance for " :<>: ShowType ( Generic all )
          :$$: Text "arising from the constraint " :<>: ShowType ( Adapt args opt all )
          )
        )
        ( () :: Constraint )
    )

-- | Throw an error message when an invalid use of 'Data.Generic.Labels.inject' is encountered:
--   - a field of the destination is missing in the source,
--   - a field that appears in both the source and destination appears more than once in either,
--   - a 'Generic' instance is missing.
type CheckInject :: Type -> Type -> Constraint
type family CheckInject small big where
  CheckInject a a = ( () :: Constraint )
  CheckInject small big =
    ( ProperInjection small big ( CollectLeaves ( Rep small ) ) ( CollectLeaves ( Rep big ) )
    , ErrorIfAmbiguous ( Rep small )
        ( TypeError
          (    Text "No instance for " :<>: ShowType ( Generic small )
          :$$: Text "arising from the constraint " :<>: ShowType ( Inject small big )
          )
        )
        ( () :: Constraint )
    , ErrorIfAmbiguous ( Rep big )
        ( TypeError
          (    Text "No instance for " :<>: ShowType ( Generic big )
          :$$: Text "arising from the constraint " :<>: ShowType ( Inject small big )
          )
        )
        ( () :: Constraint )
    )

-- | Throw an error message when an invalid use of 'Data.Generic.Labels.project' is encountered:
--   - a field of the destination is missing in the source,
--   - a field that appears in both the source and destination appears more than once in either,
--   - a 'Generic' instance is missing.
type CheckProject :: Type -> Type -> Constraint
type family CheckProject big small where
  CheckProject a a = ( () :: Constraint )
  CheckProject big small =
    ( ProperProjection big small ( CollectLeaves ( Rep big ) ) ( CollectLeaves ( Rep small ) )
    , ErrorIfAmbiguous ( Rep big )
        ( TypeError
          (    Text "No instance for " :<>: ShowType ( Generic big )
          :$$: Text "arising from the constraint " :<>: ShowType ( Project big small )
          )
        )
        ( () :: Constraint )
    , ErrorIfAmbiguous ( Rep small )
        ( TypeError
          (    Text "No instance for " :<>: ShowType ( Generic small )
          :$$: Text "arising from the constraint " :<>: ShowType ( Project big small )
          )
        )
        ( () :: Constraint )
    )

--------------------------------------------------------------------------------
-- Checking validity using type families.

data Leaves =
  Leaves
    { labelledLeaves   :: [ ( Symbol, Type ) ]
    , unlabelledLeaves :: [ Type ]
    }

type CollectLeaves :: ( Type -> Type ) -> Leaves
type family CollectLeaves f where
  CollectLeaves ( S1 ( MetaSel ( Just lbl ) _ _ _ ) ( Rec0 ty ) ) =
    'Leaves '[ '( lbl, ty ) ] '[]
  CollectLeaves ( M1 _ _ ( Rec0 ( lbl := ty ) ) ) =
    'Leaves '[ '( lbl, ty ) ] '[]
  CollectLeaves ( M1 _ _ ( Rec0 ty ) ) =
    'Leaves '[] '[ ty ]
  CollectLeaves ( M1 _ _ a ) =
    CollectLeaves a
  CollectLeaves ( l :*: r ) =
    MergeLeaves ( CollectLeaves l ) ( CollectLeaves r )
  CollectLeaves ( l :+: r ) =
    IntersectLeaves ( CollectLeaves l ) ( CollectLeaves r )
  CollectLeaves U1 =
    'Leaves '[] '[]
  CollectLeaves V1 =
    'Leaves '[] '[]

type ProperAdapt :: Type -> Type -> Type -> Leaves -> Leaves -> Leaves -> Constraint
type family ProperAdapt args opts all argLeaves optLeaves allLeaves where
  ProperAdapt args opts all ( 'Leaves ls_args us_args ) ( 'Leaves ls_opts us_opts ) ( 'Leaves ls_all us_all ) =
    ThrowMessagesWithHeader
      (    Text "No instance for "
      :$$: Text "    " :<>: ShowType ( Adapt args opts all )
      :$$: Text ""
      )
      ( CatMaybes
         '[ MessageIfNonEmpty ShowTypeWithLabelSym us_args ( Text "Unexpected unlabelled arguments:" )
          , MessageIfNonEmpty ShowTypeWithLabelSym us_opts ( Text "Unexpected unlabelled defaults:" )
          , MessageIfNonEmpty ShowTypeWithLabelSym us_all  ( Text "Unexpected unlabelled types in destination:" )
          ]
      :++:
        ValidAdaptMessages args opts all ( RelativePosition ls_opts ls_all ) ( RelativePosition ls_args ls_all )
      )

data InjectOrProject
  = InjectCase
  | ProjectCase

type ProperInjection :: Type -> Type -> Leaves -> Leaves -> Constraint
type family ProperInjection small big smallLeaves bigLeaves where
  ProperInjection small big ( 'Leaves ls_small us_small ) ( 'Leaves ls_big us_big ) =
    ThrowMessagesWithHeader
      (    Text "No instance for "
      :$$: Text "    " :<>: ShowType ( Inject small big )
      :$$: Text ""
      )
      ( CatMaybes
         '[ MessageIfNonEmpty ShowTypeWithLabelSym us_small ( Text "Unexpected unlabelled types in source:" )
          , MessageIfNonEmpty ShowTypeWithLabelSym us_big   ( Text "Unexpected unlabelled types in destination:" )
          ]
      :++:
        ValidRelativePositionMessages InjectCase ( RelativePosition ls_small ls_big )
      )

type ProperProjection :: Type -> Type -> Leaves -> Leaves -> Constraint
type family ProperProjection big small bigLeaves smallLeaves where
  ProperProjection big small ( 'Leaves ls_big us_big ) ( 'Leaves ls_small us_small ) =
    ThrowMessagesWithHeader
      (    Text "No instance for "
      :$$: Text "    " :<>: ShowType ( Project big small )
      :$$: Text ""
      )
      ( CatMaybes
         '[ MessageIfNonEmpty ShowTypeWithLabelSym us_big   ( Text "Unexpected unlabelled types in source:" )
          , MessageIfNonEmpty ShowTypeWithLabelSym us_small ( Text "Unexpected unlabelled types in destination:" )
          ]
      :++:
        ValidRelativePositionMessages ProjectCase ( RelativePosition ls_small ls_big )
      )

type ValidAdaptMessages :: Type -> Type -> Type -> RelPos ty -> RelPos ty -> [ ErrorMessage ]
type family ValidAdaptMessages args opts all opt_all_relPos args_all_relPos where
  ValidAdaptMessages args opts all ( 'RelPos optsNotInAll optsInAllDups allNotInOpt ) ( 'RelPos argsNotInAll argsInAllDups allNotInArgs ) =
    CatMaybes
     '[ MessageIfNonEmpty ShowTypeWithLabelSym argsNotInAll 
           ( Text "The following provided types do not appear in the destination:" )
      , MessageIfNonEmpty ShowTypeWithLabelSym optsNotInAll 
           ( Text "The following optional types do not appear in the destination:" )
      , MessageIfNonEmpty ShowWhichTypeWithLabelSym optsInAllDups
           ( Text "The following duplicate optional types cause a problem:" )
      , MessageIfNonEmpty ShowWhichTypeWithLabelSym argsInAllDups
           ( Text "The following duplicate provided types cause a problem:" )
      , MessageIfNonEmpty ShowTypeWithLabelSym ( allNotInOpt `Intersect` allNotInArgs )
           ( Text "The following types are non-optional but have not been provided:" )
      ]

type ValidRelativePositionMessages :: InjectOrProject -> RelPos ty -> [ ErrorMessage ]
type family ValidRelativePositionMessages injProj relPos where
  ValidRelativePositionMessages InjectCase ( 'RelPos smallNotInBig smallInBigDups _ ) =
    CatMaybes
     '[ MessageIfNonEmpty ShowWhichTypeWithLabelSym smallInBigDups
          ( Text "The following duplicate types cause a problem:" )
      , MessageIfNonEmpty ShowTypeWithLabelSym smallNotInBig
          ( Text "The following types can't be injected, as they are missing from the target:" )
      ]
  ValidRelativePositionMessages ProjectCase ( 'RelPos smallNotInBig smallInBigDups _ ) =
    CatMaybes
     '[ MessageIfNonEmpty ShowWhichTypeWithLabelSym smallInBigDups
          ( Text "The following duplicate types cause a problem:" )
      , MessageIfNonEmpty ShowTypeWithLabelSym smallNotInBig
          ( Text "The following types can't be projected out, as they are missing from the source:" )
      ]

--------------------------------------------------------------------------------
-- Computing the relative positition of two sets.

data Which
  = InSmall
  | InBig
  | InBoth

data RelPos k =
  RelPos
    { smallNotInBig  :: [ k ]
    , smallInBigDups :: [ ( k, Which ) ]
    , bigNotInSmall  :: [ k ]
    }

type RelativePosition :: [k] -> [k] -> RelPos k
type family RelativePosition small big where
  RelativePosition '[] bs = 'RelPos '[] '[] bs
  RelativePosition ( a ': as ) bs =
    RelativePositionWithRemoves a ( Remove a as ) ( Remove a bs )

type RelativePositionWithRemoves :: k -> ( [k], Mult ) -> ( [k], Mult ) -> RelPos k
type family RelativePositionWithRemoves a rem_a_as rem_a_bs where
  RelativePositionWithRemoves a '( rem_a_as, mult_a_as ) '( rem_a_bs, mult_a_bs ) =
    RelativePositionHelper a mult_a_as mult_a_bs ( RelativePosition rem_a_as rem_a_bs )

type RelativePositionHelper :: k -> Mult -> Mult -> RelPos k -> RelPos k
type family RelativePositionHelper a a_in_as a_in_bs rest where
  RelativePositionHelper a _ None ( 'RelPos smallNotInBig smallInBigDups bigNotInSmall ) =
    'RelPos ( a ': smallNotInBig ) smallInBigDups bigNotInSmall
  RelativePositionHelper a None One ( 'RelPos smallNotInBig smallInBigDups bigNotInSmall ) =
    'RelPos smallNotInBig smallInBigDups bigNotInSmall
  RelativePositionHelper a _ One ( 'RelPos smallNotInBig smallInBigDups bigNotInSmall ) =
    'RelPos smallNotInBig ( '( a, InSmall ) ': smallInBigDups ) bigNotInSmall 
  RelativePositionHelper a None Many ( 'RelPos smallNotInBig smallInBigDups bigNotInSmall ) =
    'RelPos smallNotInBig ( '( a, InBig ) ': smallInBigDups ) bigNotInSmall 
  RelativePositionHelper a _ Many ( 'RelPos smallNotInBig smallInBigDups bigNotInSmall  ) =
    'RelPos smallNotInBig ( '( a, InBoth ) ': smallInBigDups ) bigNotInSmall 

--------------------------------------------------------------------------------
-- Helpers for constructing error messages.

type ShowTypeWithLabel :: ty -> ErrorMessage
type family ShowTypeWithLabel ty where
  ShowTypeWithLabel @( Symbol, Type ) '( lbl, ty ) = Text "#" :<>: Text lbl :<>: Text " := " :<>: ShowType ty 
  ShowTypeWithLabel @k ty = ShowType ty

type ShowTypeWithLabelSym :: TyFun ty ErrorMessage
data ShowTypeWithLabelSym fun mess
type instance Eval ShowTypeWithLabelSym ty = ShowTypeWithLabel ty

type ShowWhichTypeWithLabel :: ( ty, Which ) -> ErrorMessage
type family ShowWhichTypeWithLabel tyWhich where
  ShowWhichTypeWithLabel '( ty, _ ) =  ShowTypeWithLabel ty
  --ShowWhichTypeWithLabel '( ty, which ) = ShowWhich which :<>: Text " " :<>: ShowTypeWithLabel ty

type ShowWhichTypeWithLabelSym :: TyFun ( ty, Which ) ErrorMessage
data ShowWhichTypeWithLabelSym fun mess
type instance Eval ShowWhichTypeWithLabelSym tyWhich = ShowWhichTypeWithLabel tyWhich

type ShowWhich :: Which -> ErrorMessage
type family ShowWhich which where
  ShowWhich InSmall = Text "(general)"
  ShowWhich InBig   = Text "(special)"
  ShowWhich InBoth  = Text "         "

type MergeLeaves :: Leaves -> Leaves ->Leaves
type family MergeLeaves as bs where
  MergeLeaves ( 'Leaves l1 u1 ) ( 'Leaves l2 u2 ) = 'Leaves ( l1 :++: l2 ) ( u1 :++: u2 )

type IntersectLeaves :: Leaves -> Leaves ->Leaves
type family IntersectLeaves as bs where
  IntersectLeaves ( 'Leaves l1 u1 ) ( 'Leaves l2 u2 ) = 'Leaves ( l1 `Intersect` l2 ) ( u1 `Intersect` u2 )

--------------------------------------------------------------------------------
-- Dummy class instances to de-clutter type signatures
-- by avoiding systematic expansion of class constraints.

-- | Dummy type used in dummy instances.
type Dummy :: Type
data Dummy
  deriving stock Generic

-- | Dummy instance to improve error messages.
instance {-# OVERLAPPING #-} UncheckedAdapt a a Dummy where
  uncheckedAdapt = undefined
-- | Dummy instance to improve error messages.
instance {-# OVERLAPPING #-} UncheckedAdapt a Dummy Dummy where
  uncheckedAdapt = undefined
-- | Dummy instance to improve error messages.
instance {-# OVERLAPPING #-} UncheckedAdapt a Dummy a where
  uncheckedAdapt = undefined
-- | Dummy instance to improve error messages.
instance {-# OVERLAPPING #-} UncheckedAdapt Dummy a a where
  uncheckedAdapt = undefined
-- | Dummy instance to improve error messages.
instance {-# OVERLAPPING #-} UncheckedAdapt Dummy Dummy a where
  uncheckedAdapt = undefined

-- | Dummy instance to improve error messages.
instance {-# OVERLAPPING #-} UncheckedInject a Dummy where
  uncheckedInject = undefined
-- | Dummy instance to improve error messages.
instance {-# OVERLAPPING #-} UncheckedInject Dummy a where
  uncheckedInject = undefined

-- | Dummy instance to improve error messages.
instance {-# OVERLAPPING #-} UncheckedProject a Dummy where
  uncheckedProject = undefined
-- | Dummy instance to improve error messages.
instance {-# OVERLAPPING #-} UncheckedProject Dummy a where
  uncheckedProject = undefined

-- | Dummy instance to improve error messages.
instance {-# OVERLAPPING #-} Adapt a a Dummy where
  adapt = undefined
-- | Dummy instance to improve error messages.
instance {-# OVERLAPPING #-} Adapt a Dummy Dummy where
  adapt = undefined
-- | Dummy instance to improve error messages.
instance {-# OVERLAPPING #-} Adapt a Dummy a where
  adapt = undefined
-- | Dummy instance to improve error messages.
instance {-# OVERLAPPING #-} Adapt Dummy a a where
  adapt = undefined
-- | Dummy instance to improve error messages.
instance {-# OVERLAPPING #-} Adapt Dummy Dummy a where
  adapt = undefined

-- | Dummy instance to improve error messages.
instance {-# OVERLAPPING #-} Inject a Dummy where
  inject = undefined
-- | Dummy instance to improve error messages.
instance {-# OVERLAPPING #-} Inject Dummy a where
  inject = undefined

-- | Dummy instance to improve error messages.
instance {-# OVERLAPPING #-} Project a Dummy where
  project = undefined
-- | Dummy instance to improve error messages.
instance {-# OVERLAPPING #-} Project Dummy a where
  project = undefined
