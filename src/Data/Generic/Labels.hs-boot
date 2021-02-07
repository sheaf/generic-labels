module Data.Generic.Labels
  ( Inject
    ( inject )
  , Project
    ( project )
  )
  where

--------------------------------------------------------------------------------

class Inject small big where
  inject :: small -> big -> big

class Project big small where
  project :: big -> small
