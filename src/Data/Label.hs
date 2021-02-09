{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ViewPatterns #-}

{-|
Module: Data.Label
Description: Field label type, for use with @OverloadedLabels@.

This module provides syntax for labelling values with symbolic field names.

Given @ val :: a @, we can specify a label by using the syntax
@ #field := val @, which has type @ "field" := a @.

For instance, we can pass a record of three arguments with the syntax:

@
myRecord :: ( "field1" := Int, "field2" := Bool, "field3" := Float )
myRecord = ( #field1 := 3, #field2 := True, #field3 := 7.7 )
@

This is a simple triple of labelled types, so the order matters.    

However, this library provides functionality which will automatically
handle re-ordering fields when needed, see "Data.Generic.Labels".
-}

module Data.Label
  ( (:=)
      ( .., (:=) )
  , Label
      ( Label )
  ) where

-- base
import Data.Kind
  ( Type )
import GHC.Exts
  ( proxy# )
import GHC.OverloadedLabels
  ( IsLabel
    ( fromLabel )
  )
import GHC.TypeLits
  ( Symbol, KnownSymbol, symbolVal' )

--------------------------------------------------------------------------------
-- Field labels.

-- | 'Data.Proxy.Proxy'-like label type,
-- used to pass the label name at the type-level.
--
-- With @OverloadedLabels@:
--
-- @ #foo :: Label "foo" @
data Label ( lbl :: Symbol ) = Label
type role Label nominal
instance ( lbl' ~ lbl ) => IsLabel lbl ( Label lbl' ) where
  fromLabel = Label
instance KnownSymbol lbl => Show ( Label lbl ) where
  show _ = "#" <> symbolVal' @lbl proxy#

-- | A type with a 'Label'.
--
-- With @OverloadedLabels@:
--
-- @ ( #bar := Just \'c\' ) :: ( "bar" := Maybe Char ) @
newtype ( lbl :: Symbol ) := ( a :: Type ) = Labelled { unLabel :: a }

instance ( KnownSymbol lbl, Show a ) => Show ( lbl := a ) where
  showsPrec p ( Labelled a ) =
    showParen ( p > 1 )
      ( showString ( show ( Label @lbl ) <> " := " ) . showsPrec 2 a )

infix 1 :=
-- | Add a 'Label' to a type.
--
-- With @OverloadedLabels@:
--
-- @ ( #bar := Just \'c\' ) :: ( "bar" := Maybe Char ) @
pattern (:=) :: Label lbl -> a -> lbl := a
pattern lbl := a <- ( ( \ ( Labelled a ) -> LabelPair Label a ) -> LabelPair lbl a )
  where
    _ := a = Labelled a
{-# COMPLETE (:=) #-}

data LabelPair lbl a = LabelPair !( Label lbl ) !a
