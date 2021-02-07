{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

{-|
Module: Data.Generic.Labels.Internal.Errors

Internal module providing custom type errors for
invalid uses of 'Data.Generic.Labels.inject' or 'Data.Generic.Labels.project'.

Consider for instance trying to project onto a smaller target record,
but the source record is missing one of the fields

@
missingField :: ( "a" := Bool, "c" := Double ) -> ( "c" := Double, "b" := Int )
missingField = project
@

Note that the source is missing the @ "b" := Int @ field which is present in the target.

This results in the following error message:

@
  * 'project': no instance for
        Project
          ("b" := Float, "a" := Bool, "c" := Double)
          ("c" := Double, "b" := Int)
    The type being projected down is missing the following fields:
      - #b := Int
@
-}

module Data.Generic.Labels.Internal.Errors
  (  InjectErrorMessage,  InjectLabelMessage
  , ProjectErrorMessage, ProjectLabelMessage
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

-- generic-labels
import Data.Label
  ( (:=) )
import Data.Type.Error
  ( ErrorIfAmbiguous )
import Data.Type.List
  ( (:++:), Remove )
import Data.Type.Multiplicity
  ( Mult ( None, One, Many ) )
import {-# SOURCE #-} Data.Generic.Labels
  ( Inject, Project )

--------------------------------------------------------------------------------
-- Helper type families for improved error messages.

-- | Throw an error message when an invalid use of 'Data.Generic.Labels.inject' is encountered:
--   - a field of the destination is missing in the source,
--   - a field that appears in both the source and destination appears more than once in either,
--   - a 'Generic' instance is missing.
type InjectErrorMessage :: Type -> Type -> Constraint
type family InjectErrorMessage small big where
  InjectErrorMessage small big =
    ( ProperInjection small big ( CollectLeaves ( Rep small ) ) ( CollectLeaves ( Rep big ) )
    , ErrorIfAmbiguous ( Rep small )
        ( TypeError
          (    Text "'inject': no instance for " :<>: ShowType ( Generic small )
          :$$: Text "arising from the constraint " :<>: ShowType ( Inject small big ) :<>: Text "."
          )
        )
        ( () :: Constraint )
    , ErrorIfAmbiguous ( Rep big )
        ( TypeError
          (    Text "'inject': no instance for " :<>: ShowType ( Generic big )
          :$$: Text "arising from the constraint " :<>: ShowType ( Inject small big ) :<>: Text "."
          )
        )
        ( () :: Constraint )
    )

-- | Throw an error message when an invalid use of 'Data.Generic.Labels.project' is encountered:
--   - a field of the destination is missing in the source,
--   - a field that appears in both the source and destination appears more than once in either,
--   - a 'Generic' instance is missing.
type ProjectErrorMessage :: Type -> Type -> Constraint
type family ProjectErrorMessage big small where
  ProjectErrorMessage big small =
    ( ProperProjection big small ( CollectLeaves ( Rep big ) ) ( CollectLeaves ( Rep small ) )
    , ErrorIfAmbiguous ( Rep big )
        ( TypeError
          (    Text "'project': no instance for " :<>: ShowType ( Generic big )
          :$$: Text "arising from the constraint " :<>: ShowType ( Project big small ) :<>: Text "."
          )
        )
        ( () :: Constraint )
    , ErrorIfAmbiguous ( Rep small )
        ( TypeError
          (    Text "'project': no instance for " :<>: ShowType ( Generic small )
          :$$: Text "arising from the constraint " :<>: ShowType ( Project big small ) :<>: Text "."
          )
        )
        ( () :: Constraint )
    )

-- | Throw an error message on use of 'Data.Generic.Labels.inject'
-- when encountering two distinct types with the same label.
type InjectLabelMessage :: Symbol -> Type -> Type -> Constraint
type family InjectLabelMessage lbl small big where
  InjectLabelMessage _   ty    ty  = ( () :: Constraint )
  InjectLabelMessage lbl small big =
    TypeError
      (    Text "'inject': mismatched types at label #" :<>: Text lbl :<>: Text "."
      :$$: Text "   Expected type: " :<>: ShowType big
      :$$: Text "     Actual type: " :<>: ShowType small
      )

-- | Throw an error message on use of 'Data.Generic.Labels.project'
-- when encountering two distinct types with the same label.
type ProjectLabelMessage :: Symbol -> Type -> Type -> Constraint
type family ProjectLabelMessage lbl big small where
  ProjectLabelMessage _   ty  ty    = ( () :: Constraint )
  ProjectLabelMessage lbl big small =
    TypeError
      (    Text "'project': mismatched types at label #" :<>: Text lbl :<>: Text "."
      :$$: Text "   Expected type: " :<>: ShowType small
      :$$: Text "     Actual type: " :<>: ShowType big
      )

--------------------------------------------------------------------------------
-- Internal types and type families.

data Leaves =
  Leaves
    { labelledLeaves   :: [ ( Symbol, Type ) ]
    , unlabelledLeaves :: [ Type ]
    }

type CollectLeaves :: ( Type -> Type ) -> Leaves
type family CollectLeaves f where
  CollectLeaves ( M1 _ _ ( Rec0 ( lbl := ty ) ) ) =
    'Leaves '[ '( lbl, ty ) ] '[]
  CollectLeaves ( S1 ( MetaSel ( Just lbl ) _ _ _ ) ( Rec0 ty ) ) =
    'Leaves '[ '( lbl, ty ) ] '[]
  CollectLeaves ( M1 _ _ ( Rec0 ty ) ) =
    'Leaves '[] '[ ty ]
  CollectLeaves ( M1 _ _ a ) =
    CollectLeaves a
  CollectLeaves ( l :*: r ) =
    MergeLeaves ( CollectLeaves l ) ( CollectLeaves r )
  CollectLeaves _ =
    'Leaves '[] '[]

data InjectOrProject
  = InjectCase
  | ProjectCase

type ProperInjection :: Type -> Type -> Leaves -> Leaves -> Constraint
type family ProperInjection small big smallLeaves bigLeaves where
  ProperInjection small big ( 'Leaves ls_small us_small ) ( 'Leaves ls_big us_big ) =
    ( ErrorUnlessValidRelativePosition InjectCase small big ( RelativePosition ls_small ls_big )
    , ErrorUnlessValidRelativePosition InjectCase small big ( RelativePosition us_small us_big )
    )

type ProperProjection :: Type -> Type -> Leaves -> Leaves -> Constraint
type family ProperProjection big small bigLeaves smallLeaves where
  ProperProjection big small ( 'Leaves ls_big us_big ) ( 'Leaves ls_small us_small ) =
    ( ErrorUnlessValidRelativePosition ProjectCase small big ( RelativePosition ls_small ls_big )
    , ErrorUnlessValidRelativePosition ProjectCase small big ( RelativePosition us_small us_big )
    )

type ErrorUnlessValidRelativePosition :: InjectOrProject -> Type -> Type -> RelPos ty -> Constraint
type family ErrorUnlessValidRelativePosition injProj small big relPos where
  ErrorUnlessValidRelativePosition injProj small big ( 'RelPos smallNotInBig smallInBigDups ) =
    ( ErrorUnlessNoDups injProj small big smallInBigDups
    , ErrorUnlessEmpty  injProj small big smallNotInBig
    )

type ErrorUnlessEmpty :: InjectOrProject -> Type -> Type -> [ ty ] -> Constraint
type family ErrorUnlessEmpty injProj small big smallNotInBig where
  ErrorUnlessEmpty _ _ _ '[] = ( () :: Constraint )
  ErrorUnlessEmpty InjectCase small big smallNotInBig =
    TypeError
      (    Text "'inject': no instance for "
      :$$: Text "    " :<>: ShowType ( Inject small big )
      :$$: Text "The following fields cannot be injected:"
      :$$: ShowMissingTypes smallNotInBig
      )
  ErrorUnlessEmpty ProjectCase small big smallNotInBig =
    TypeError
      (    Text "'project': no instance for "
      :$$: Text "    " :<>: ShowType ( Project big small )
      :$$: Text "The type being projected down is missing the following fields:"
      :$$: ShowMissingTypes smallNotInBig
      )

type ErrorUnlessNoDups :: InjectOrProject -> Type -> Type -> [ ( ty, Which ) ] -> Constraint
type family ErrorUnlessNoDups injProj small big smallInBigDups where
  ErrorUnlessNoDups _ _ _ '[] = ( () :: Constraint )
  ErrorUnlessNoDups InjectCase small big smallInBigDups =
    TypeError
      (    Text "'inject': no instance for "
      :$$: Text "    " :<>: ShowType ( Inject small big )
      :$$: Text "The following duplicate fields cause a problem:"
      :$$: ShowMissingWhichTypes smallInBigDups
      )
  ErrorUnlessNoDups ProjectCase small big smallInBigDups =
    TypeError
      (    Text "'project': no instance for "
      :$$: Text "    " :<>: ShowType ( Project big small )
      :$$: Text "The following duplicate fields cause a problem:"
      :$$: ShowMissingWhichTypes smallInBigDups
      )

type ShowMissingTypes :: [ ty ] -> ErrorMessage
type family ShowMissingTypes tys where
  ShowMissingTypes '[] =
    Text ""
  ShowMissingTypes @( Symbol, Type ) ( '( lbl, ty ) ': rest ) =
    Text "  - #" :<>: Text lbl :<>: Text " := " :<>: ShowType ty :$$: ShowMissingTypes rest
  ShowMissingTypes @Type ( ty ': rest ) =
    Text "  - " :<>: ShowType ty :$$: ShowMissingTypes rest

type ShowMissingWhichTypes :: [ ( ty, Which ) ] -> ErrorMessage
type family ShowMissingWhichTypes tys where
  ShowMissingWhichTypes '[] = Text ""
  ShowMissingWhichTypes @( Symbol, Type ) ( '( '( lbl, ty ), which ) ': rest ) =
    Text "  - " :<>: ShowWhich which :<>: Text " #" :<>: Text lbl :<>: Text " := " :<>: ShowType ty :$$: ShowMissingWhichTypes rest
  ShowMissingWhichTypes @Type ( '( ty, which ) ': rest ) =
    Text "  - " :<>: ShowWhich which :<>: Text " " :<>: ShowType ty :$$: ShowMissingWhichTypes rest

type ShowWhich :: Which -> ErrorMessage
type family ShowWhich which where
  ShowWhich InSmall = Text "[Small]"
  ShowWhich InBig   = Text "[ Big ]"
  ShowWhich InBoth  = Text "[ Both]"

type MergeLeaves :: Leaves -> Leaves ->Leaves
type family MergeLeaves as bs where
  MergeLeaves ( 'Leaves l1 u1 ) ( 'Leaves l2 u2 ) = 'Leaves ( l1 :++: l2 ) ( u1 :++: u2 )

data Which
  = InSmall
  | InBig
  | InBoth

data RelPos k
  = RelPos
      { smallNotInBig  :: [ k ]
      , smallInBigDups :: [ ( k, Which ) ]
      }

type RelativePosition :: [k] -> [k] -> RelPos k
type family RelativePosition small big where
  RelativePosition '[] _ = 'RelPos '[] '[]
  RelativePosition ( a ': as ) bs =
    RelativePositionWithRemoves a ( Remove a as ) ( Remove a bs )

type RelativePositionWithRemoves :: k -> ( [k], Mult ) -> ( [k], Mult ) -> RelPos k
type family RelativePositionWithRemoves a rem_a_as rem_a_bs where
  RelativePositionWithRemoves a '( rem_a_as, mult_a_as ) '( rem_a_bs, mult_a_bs ) =
    RelativePositionHelper a mult_a_as mult_a_bs ( RelativePosition rem_a_as rem_a_bs )

type RelativePositionHelper :: k -> Mult -> Mult -> RelPos k -> RelPos k
type family RelativePositionHelper a a_in_as a_in_bs rest where
  RelativePositionHelper a _ None ( 'RelPos smallNotInBig smallInBigDups ) =
    'RelPos ( a ': smallNotInBig ) smallInBigDups
  RelativePositionHelper a None One ( 'RelPos smallNotInBig smallInBigDups ) =
    'RelPos smallNotInBig smallInBigDups
  RelativePositionHelper a _ One ( 'RelPos smallNotInBig smallInBigDups ) =
    'RelPos smallNotInBig ( '( a, InSmall ) ': smallInBigDups )
  RelativePositionHelper a None Many ( 'RelPos smallNotInBig smallInBigDups ) =
    'RelPos smallNotInBig ( '( a, InBig ) ': smallInBigDups )
  RelativePositionHelper a _ Many ( 'RelPos smallNotInBig smallInBigDups ) =
    'RelPos smallNotInBig ( '( a, InBoth ) ': smallInBigDups )
