{-# OPTIONS_GHC -fplugin Brisk.Plugin #-}
{-# LANGUAGE TemplateHaskell #-}
module GHC.Base.Brisk
  ( apSpec
  , flipSpec
  , composeSpec
  ) where
import Control.Monad
import Brisk.Annotations
{-# ANN module SpecModule #-}
{-# ANN apSpec      (Assume '($)) #-}
{-# ANN composeSpec (Assume '(.)) #-}
{-# ANN flipSpec    (Assume 'flip) #-}
apSpec :: (a -> b) -> a -> b
apSpec f x = f x

composeSpec :: (b -> c) -> (a -> b) -> a -> c
composeSpec f g x = f (g x)

flipSpec :: (a -> b -> c) -> b -> a -> c
flipSpec f x y = f y x
