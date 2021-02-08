{-# LANGUAGE UndecidableInstances #-}

module Data.Type.Error
  ( ErrorIfAmbiguous
  , ListBulletsWith
  , MessageIfNonEmpty
  , ThrowMessagesWithHeader
  , UnlinesMessages
  )
  where

-- base
import Data.Kind
  ( Constraint )
import GHC.TypeLits
  ( TypeError, ErrorMessage(..) )

-- generic-lens-core
import Data.Generics.Product.Internal.GLens
  ( Eval, TyFun )

--------------------------------------------------------------------------------

type ErrorIfAmbiguous :: k -> Constraint -> l -> l
type family ErrorIfAmbiguous break err a where
  ErrorIfAmbiguous Dummy _ _ = Dummy
  ErrorIfAmbiguous _     _ a = a

type Dummy :: k
data family Dummy

type MessageIfNonEmpty :: TyFun ty ErrorMessage -> [ ty ] -> ErrorMessage -> Maybe ErrorMessage
type family MessageIfNonEmpty showTySym tys message where
  MessageIfNonEmpty _         '[] _       = Nothing
  MessageIfNonEmpty showTySym tys message = Just ( message :$$: ListBulletsWith showTySym tys )

type ListBulletsWith :: TyFun ty ErrorMessage -> [ ty ] -> ErrorMessage
type family ListBulletsWith showTySym tys where
  ListBulletsWith _         '[]           = Text ""
  ListBulletsWith showTySym ( ty ': tys ) = Text "  - " :<>: Eval showTySym ty
                                       :$$: ListBulletsWith showTySym tys

type ThrowMessagesWithHeader :: ErrorMessage -> [ ErrorMessage ] -> Constraint
type family ThrowMessagesWithHeader header messages where
  ThrowMessagesWithHeader _ '[] = ( () :: Constraint )
  ThrowMessagesWithHeader header messages = TypeError ( header :$$: UnlinesMessages messages )

type UnlinesMessages :: [ ErrorMessage ] -> ErrorMessage
type family UnlinesMessages messages where
  UnlinesMessages '[] = Text ""
  UnlinesMessages ( m ': ms ) = m :$$: UnlinesMessages ms
