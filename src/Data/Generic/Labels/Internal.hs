{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Generic.Labels.Internal
  ( GAdapt(..) )
  where

-- base
import Data.Kind
  ( Constraint, Type )
import GHC.Generics
import GHC.TypeLits
  ( Symbol )

-- generic-lens-core
import Data.Generics.Product.Internal.GLens
  ( Eval, GLens(..), GLens', TyFun )
import Data.Generics.Internal.Profunctor.Lens
  ( view )
import Data.Generics.Internal.Profunctor.Iso 
  ( Iso, iso, kIso )

-- generic-labels
import Data.Label
  ( (:=)(..), Label(..) )
import Data.Generic.Labels.Internal.Errors
  ( AdaptLabelMessage )

--------------------------------------------------------------------------------
-- Generics machinery for 'Adapt'.

type GAdapt :: ( Type -> Type ) -> ( Type -> Type ) -> ( Type -> Type ) -> Constraint
class GAdapt args opt all where
  gAdapt :: args p -> opt p -> all p

instance ( GAdapt args opt all1, GAdapt args opt all2 ) => GAdapt args opt ( all1 :*: all2 ) where
  gAdapt args opt = gAdapt args opt :*: gAdapt args opt

instance GAdapt args opt all => GAdapt args opt ( C1 c all ) where
  gAdapt args opt = M1 $ gAdapt args opt

instance GAdapt args opt all => GAdapt args opt ( D1 c all ) where
  gAdapt args opt = M1 $ gAdapt args opt

-- | This instance is INCOHERENT because we assume that no type variable (say @all0@)
-- will later be instantiated to a labelled type @lbl := all@.
--
-- The end result is that, when we have both a built-in Haskell record field name
-- as well as an explicit label, we prioritise the built-in record field name over the label.
instance {-# INCOHERENT #-}
         ( GLens' ( HasTotalLabelPSym lbl ) ( args :*: opts ) all )
      => GAdapt args opts ( M1 m meta ( Rec0 ( lbl := all ) ) )
      where
  gAdapt args opt = M1 . K1 . ( Label @lbl := ) $ view ( glens @( HasTotalLabelPSym lbl ) ) ( args :*: opt )

instance ( GLens' ( HasTotalLabelPSym lbl ) ( args :*: opts ) all )
      => GAdapt args opts ( S1 ( MetaSel ( Just lbl ) p f b ) ( Rec0 all ) )
      where
  gAdapt args opt = M1 . K1 $ view ( glens @( HasTotalLabelPSym lbl ) ) ( args :*: opt )

--------------------------------------------------------------------------------
-- Generic lens machinery.

type And :: Maybe a -> Maybe a -> Maybe a
type family m1 `And` m2 where
  Just a `And` Just a = Just a
  _      `And` _      = Nothing

type Or :: Maybe a -> Maybe a -> Maybe a
type family m1 `Or` m2 where
  Just a `Or` _ = Just a
  _      `Or` b = b

type HasTotalLabelP :: Symbol -> ( Type -> Type ) -> Maybe Type
type family HasTotalLabelP lbl f where
  HasTotalLabelP lbl ( S1 ( MetaSel ( Just lbl ) _ _ _ ) ( Rec0 ty ) ) = 
    Just ty
  HasTotalLabelP lbl ( S1 ( MetaSel ( Just lbl' ) _ _ _ ) _ ) = 
    Nothing
  HasTotalLabelP lbl ( S1 _ ( K1 _ ( lbl := ty ) ) ) =
    Just ty
  HasTotalLabelP lbl ( S1 _ ( K1 _ ( lbl' := _ ) ) ) =
    Nothing
  HasTotalLabelP lbl ( l :*: r ) =
    HasTotalLabelP lbl l `Or` HasTotalLabelP lbl r
  HasTotalLabelP lbl ( l :+: r ) =
    HasTotalLabelP lbl l `And` HasTotalLabelP lbl r
  HasTotalLabelP lbl ( S1 _ _ ) =
    Nothing
  HasTotalLabelP lbl ( C1 _ f ) =
    HasTotalLabelP lbl f
  HasTotalLabelP lbl ( D1 _ f ) =
    HasTotalLabelP lbl f
  HasTotalLabelP lbl ( K1 _ _ ) =
    Nothing
  HasTotalLabelP lbl U1 =
    Nothing
  HasTotalLabelP lbl V1 =
    Nothing

type HasTotalLabelPSym :: Symbol -> TyFun ( Type -> Type ) ( Maybe Type )
data HasTotalLabelPSym lbl f mbTy
type instance Eval ( HasTotalLabelPSym lbl ) f = HasTotalLabelP lbl f

class LabelIso mbLbl1 mbLbl2 s t a b | mbLbl1 s -> a, mbLbl2 t -> b where
  lblIso :: Iso s t a b
instance
  ( AdaptLabelMessage lbl ( Just a1 ) Nothing b1
  , a1 ~ a, b1 ~ b
  ) => LabelIso ( Just lbl ) ( Just lbl ) ( lbl := a1 ) ( lbl := b1 ) a b where
  lblIso = iso ( \ ( _ := a ) -> a ) ( Label @lbl := )
  {-# INLINE lblIso #-}
instance LabelIso Nothing Nothing a b a b where
  lblIso = id
  {-# INLINE lblIso #-}

type GetLabel :: Type -> Maybe Symbol
type family GetLabel ty where
 GetLabel ( lbl := _ ) = Just lbl
 GetLabel _            = Nothing

instance {-# OVERLAPPABLE #-} LabelIso ( GetLabel a' ) ( GetLabel b' ) a' b' a b
      => GLens pred ( K1 r a' ) ( K1 r b' ) a b where
  glens = kIso . lblIso @( GetLabel a' ) @( GetLabel b' )
  {-# INLINE glens #-}
