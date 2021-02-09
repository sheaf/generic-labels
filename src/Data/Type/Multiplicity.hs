{-# LANGUAGE StandaloneKindSignatures #-}

module Data.Type.Multiplicity
  ( AddMult, Mult(..) )
  where

--------------------------------------------------------------------------------

data Mult
  = None
  | One
  | Many

type AddMult :: Mult -> Mult -> Mult
type family AddMult m1 m2 where
  AddMult None m = m
  AddMult m None = m
  AddMult _ _    = Many
