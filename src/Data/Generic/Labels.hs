{-# LANGUAGE UndecidableInstances #-}

module Data.Generic.Labels
  ( Inject
    ( inject )
  , Project
    ( project )
  , withOptionalArguments
  )
  where

-- base
import GHC.Generics
  ( Generic
    ( Rep, from, to )
  )

-- generic-labels
import Data.Label
  ( (:=) )
import Data.Generic.Labels.Internal
  ( GInject
    ( ginject )
  , GProject
    ( gproject )
  )
import Data.Generic.Labels.Internal.Errors
  (  InjectErrorMessage,  InjectLabelMessage
  , ProjectErrorMessage, ProjectLabelMessage
  )

--------------------------------------------------------------------------------
-- Inject/project classes and instances.


-- | 'inject' a smaller type into a large one, overriding the fields
-- in the larger type with those from the smaller type.
class Inject small big where
  inject :: small -> big -> big

-- | 'project' a smaller type out from a larger one, discarding the rest.
class Project big small where
  project :: big -> small

instance {-# OVERLAPPING #-} Inject  a a where
  inject  = const
instance {-# OVERLAPPING #-} Project a a where
  project = id
instance ( InjectLabelMessage  lbl a b, a ~ b ) => Inject  ( lbl := a ) ( lbl := b ) where
  inject  = const
instance ( ProjectLabelMessage lbl a b, a ~ b ) => Project ( lbl := a ) ( lbl := b ) where
  project = id

instance {-# OVERLAPPABLE #-}
    ( Generic small, Generic big
    , GInject ( Rep small ) ( Rep big )
    , InjectErrorMessage small big
    )
  => Inject small big where
  inject small big = to $ ginject ( from small ) ( from big )

instance {-# OVERLAPPABLE #-}
    ( Generic big, Generic small
    , GProject ( Rep big ) ( Rep small )
    , ProjectErrorMessage big small
    )
  => Project big small where
  project = to . gproject . from

--------------------------------------------------------------------------------
-- Handling functions with optional arguments.

-- | Create an adapter for a function that has required and optional arguments.
--
-- Takes a function @ f :: ( req, opt ) -> r @
-- which has both required and optional arguments,
-- together with default values @ def :: opt @ for the optional arguments,
-- and returns a function 
withOptionalArguments
  :: forall args r req opt
  .  ( Project args req, Inject args ( req, opt ) )
  => opt
  -> ( ( req, opt ) -> r )
  -> ( args -> r )
withOptionalArguments opt f args = f ( inject args ( project args, opt ) )
