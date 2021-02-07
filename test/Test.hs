{-# LANGUAGE OverloadedLabels #-}

module Test where

-- generic-labels
import Data.Label
  ( (:=)(..) )
import Data.Generic.Labels
  ( Inject(..), Project(..) )

--------------------------------------------------------------------------------
-- Testing.

-----------------------
-- Testing 'project'.

projOK1 :: ( "b" := Float, "c" := Int, "a" := Double ) -> ( "a" := Double, "b" := Float )
projOK1 = project

projOK2 :: ( Float, Int ) -> ( Int, Float )
projOK2 = project

projOK3 :: ( "b" := Int, "a" := Float )
projOK3 = project ( #a := 17.7, #b := 9 )

-- * 'project': no instance for
--       Project
--         ("b" := Float, "a" := Bool, "c" := Double)
--         ("c" := Double, "b" := Int)
--   The type being projected down is missing the following fields:
--     - #b := Int
projErr1 :: ( "b" := Float, "a" := Bool, "c" := Double ) -> ( "c" := Double, "b" := Int )
projErr1 = project

--  * 'project': no instance for
--        Project
--          ("b" := Float, "c" := Int, "a" := Bool)
--          ("a" := Bool, "b" := Float, "d" := Int)
--    The type being projected down is missing the following fields:
--      - #d := Int
projErr2 :: ( "b" := Float, "c" := Int, "a" := Bool ) -> ( "a" := Bool, "b" := Float, "d" := Int )
projErr2 = project

--  * 'project': no instance for
--        Project (Float, Float, Int) (Int, Float)
--    The following duplicate fields cause a problem:
--      - [ Big ] Float
projErr3 :: ( Float, Float, Int ) -> ( Int, Float )
projErr3 = project

--  * 'project': no instance for
--        Project (Float, Int) (Int, Float, Int)
--    The following duplicate fields cause a problem:
--      - [Small] Int
projErr4 :: ( Float, Int ) -> ( Int, Float, Int )
projErr4 = project

-- * No instance for (Fractional Int) arising from the literal `17.7'
projErr5 :: ( "b" := Int, "a" := Int )
projErr5 = project ( #a := 17.7, #b := 9 )

--  * 'project': no instance for
--        Project
--          ("a" := Bool, "b" := Int, "b" := Int) ("b" := Int, "b" := Int)
--    The following duplicate fields cause a problem:
--      - [ Both] #b := Int
projErr6 :: ( "a" := Bool, "b" := Int, "b" := Int ) -> ( "b" := Int, "b" := Int ) 
projErr6 = project

-----------------------
-- Testing 'inject'.

injOK1 :: ( "a" := Double, "b" := Float ) -> ( "b" := Float, "c" := Int, "a" := Double ) -> ( "b" := Float, "c" := Int, "a" := Double )
injOK1 = inject

injOK2 :: ( Float, Int ) -> ( Int, Float, Double ) -> ( Int, Float, Double )
injOK2 = inject
