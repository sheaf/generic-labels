{-# LANGUAGE RoleAnnotations #-}

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
      ( (:=) )
  , Label
      ( Label )
  ) where

-- base
import Data.Kind
  ( Type )
import GHC.OverloadedLabels
  ( IsLabel
    ( fromLabel )
  )
import GHC.TypeLits
  ( Symbol )

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

-- | A type with a 'Label'.
--
-- With @OverloadedLabels@:
--
-- @ ( #bar := Just 'c' ) :: ( "bar" := Maybe Char ) @
data ( lbl :: Symbol ) := ( a :: Type ) = Label lbl := a
