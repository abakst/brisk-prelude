{-# OPTIONS_GHC -fplugin Brisk.Plugin #-}
{-# LANGUAGE TemplateHaskell #-}
module Control.Distributed.BriskStatic.Brisk
  (castEffectSpec
  ) where
import Control.Distributed.BriskStatic.Internal (castEffect)
import Brisk.Annotations
{- ANN module SpecModule #-}
{- ANN castEffectSpec (Assume 'castEffect) #-}
castEffectSpec :: a -> b -> a
castEffectSpec p _ = p
