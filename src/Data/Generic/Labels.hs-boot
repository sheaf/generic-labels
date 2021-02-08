{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Data.Generic.Labels where

--------------------------------------------------------------------------------

class UncheckedAdapt args opt all where
   uncheckedAdapt :: args -> opt -> all
class UncheckedAdapt small big big
   => UncheckedInject  small big where
   uncheckedInject :: small -> big -> big
class UncheckedAdapt big big small
  => UncheckedProject big small where
   uncheckedProject :: big -> small

class ( UncheckedAdapt args opt all )
   => Adapt args opt all where
   adapt :: args -> opt -> all
class ( UncheckedInject small big )
   => Inject small big where
   inject :: small -> big -> big
class ( UncheckedProject big small )
   => Project big small where
   project :: big -> small
