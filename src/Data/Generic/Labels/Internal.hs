{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Generic.Labels.Internal
  ( GInject(..), GProject(..) )
  where

-- base
import Data.Kind
  ( Constraint, Type )
import GHC.Generics
import GHC.TypeLits
  ( Symbol )

-- generic-lens-core
import Data.Generics.Internal.Families
  ( HasTotalTypeP )
import Data.Generics.Product.Internal.GLens
  ( Eval, GLens(..), GLens', TyFun )
import Data.Generics.Internal.Profunctor.Lens
  ( view )

-- generic-labels
import Data.Label
  ( (:=) )

--------------------------------------------------------------------------------
-- Generics machinery for 'Inject'.

type GInject :: ( Type -> Type ) -> ( Type -> Type ) -> Constraint
class GInject small big where
  ginject :: small p -> big p -> big p

instance ( GInject small big1, GInject small big2 ) => GInject small ( big1 :*: big2 ) where
  ginject small ( big1 :*: big2 ) = ginject small big1 :*: ginject small big2

instance GInject small big => GInject small ( C1 c big ) where
  ginject small ( M1 big ) = M1 ( ginject small big )

instance GInject small big => GInject small ( D1 c big ) where
  ginject small ( M1 big ) = M1 ( ginject small big )


instance {-# OVERLAPPING #-}
         ( leaf ~ M1 m meta ( Rec0 ( lbl := big ) )
         , GInjectLeaf small leaf ( HasTotalLabelP lbl small )
         )
      => GInject small ( M1 m meta ( Rec0 ( lbl := big ) ) ) where
  ginject = ginjectLeaf @_ @_ @( HasTotalLabelP lbl small )

{-
instance
         ( leaf ~ S1 ( MetaSel ( Just lbl ) p f b ) ( Rec0 big )
         , GInjectLeaf small leaf ( HasTotalLabelP lbl small )
         )
      => GInject small ( S1 ( MetaSel ( Just lbl ) p f b ) ( Rec0 big ) ) where
  ginject = ginjectLeaf @_ @_ @( HasTotalLabelP lbl small )
-}

instance {-# OVERLAPPABLE #-}
         ( leaf ~ M1 m meta ( Rec0 big )
         , GInjectLeaf small leaf ( HasTotalTypeP big small )
         )
      => GInject small ( M1 m meta ( Rec0 big ) ) where
  ginject = ginjectLeaf @_ @_ @( HasTotalTypeP big small )


type GInjectLeaf :: ( Type -> Type ) -> ( Type -> Type ) -> Maybe Type -> Constraint
class GInjectLeaf small big mbLeaf where
  ginjectLeaf :: small p -> big p -> big p

instance {-# OVERLAPPING #-}
     ( GLens' ( HasTotalLabelPSym lbl ) small ( lbl := leaf ), ty ~ leaf )
  => GInjectLeaf small ( M1 m meta ( Rec0 ( lbl := leaf ) ) ) ( Just ty ) where
  ginjectLeaf small _ = M1 . K1 $ view ( glens @( HasTotalLabelPSym lbl ) ) small

{-
instance
     ( GLens' ( HasTotalLabelPSym lbl ) small leaf, ty ~ leaf )
  => GInjectLeaf small ( S1 ( MetaSel ( Just lbl ) p f b ) ( Rec0 leaf ) ) ( Just ty ) where
  ginjectLeaf small _ = M1 . K1 $ view ( glens @( HasTotalLabelPSym lbl ) ) small
-}

instance {-# OVERLAPPABLE #-}
     ( GLens' ( HasTotalTypePSym leaf ) small leaf, ty ~ leaf )
  => GInjectLeaf small ( M1 m meta ( Rec0 leaf ) ) ( Just ty ) where
  ginjectLeaf small _ = M1 . K1 $ view ( glens @( HasTotalTypePSym leaf ) ) small

instance GInjectLeaf small ( M1 m meta ( Rec0 leaf ) ) Nothing where
  ginjectLeaf = const id

--------------------------------------------------------------------------------
-- Generics machinery for 'Project'.

type GProject :: ( Type -> Type ) -> ( Type -> Type ) -> Constraint
class GProject big small where
  gproject :: big p -> small p

instance ( GProject big small1, GProject big small2 ) => GProject big ( small1 :*: small2 ) where
  gproject big = gproject big :*: gproject big

instance ( GProject big small ) => GProject big ( C1 c small ) where
  gproject = M1 . gproject

instance ( GProject big small ) => GProject big ( D1 c small ) where
  gproject = M1 . gproject

instance  {-# OVERLAPPING #-}
         ( GLens' ( HasTotalLabelPSym lbl ) big ( lbl := small ) )
      => GProject big ( M1 m meta ( Rec0 ( lbl := small ) ) )
      where
  gproject big = M1 . K1 $ view ( glens @( HasTotalLabelPSym lbl ) ) big

{-
instance ( GLens' ( HasTotalLabelPSym lbl ) big small )
      => GProject big ( S1 ( MetaSel ( Just lbl ) p f b ) ( Rec0 small ) )
      where
  gproject big = M1 . K1 $ view ( glens @( HasTotalLabelPSym lbl ) ) big
-}

instance {-# OVERLAPPABLE #-}
         ( GLens' ( HasTotalTypePSym small ) big small )
      => GProject big ( M1 m meta ( Rec0 small ) )
      where
  gproject big = M1 . K1 $ view ( glens @( HasTotalTypePSym small ) ) big

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
  HasTotalLabelP lbl ( S1 _ ( K1 _ ( lbl := ty ) ) ) =
    Just ty
  HasTotalLabelP lbl ( S1 ( MetaSel ( Just lbl ) _ _ _ ) ( Rec0 ty ) ) = 
    Just ty
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

type HasTotalTypePSym :: Type -> TyFun ( Type -> Type ) ( Maybe Type )
data HasTotalTypePSym ty f mbTy
type instance Eval ( HasTotalTypePSym ty ) f = HasTotalTypeP ty f
