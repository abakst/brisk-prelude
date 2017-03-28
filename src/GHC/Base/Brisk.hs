{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
module GHC.Base.Brisk
  ( apSpec
  , flipSpec
  , composeSpec
  , maybeSpec
  , buildSpec
  ) where
import Control.Monad
import Brisk.Annotations
import Data.Maybe (maybe)
import GHC.Base (build)
{-# ANN module SpecModule #-}
{-# ANN apSpec      (Assume '($)) #-}
{-# ANN composeSpec (Assume '(.)) #-}
{-# ANN flipSpec    (Assume 'flip) #-}
{-# ANN buildSpec   (Assume 'build) #-}
{-# ANN maybeSpec   (Assume 'maybe) #-}
apSpec :: forall k a b. (a -> b) -> a -> b
apSpec f x = f x

composeSpec :: (b -> c) -> (a -> b) -> a -> c
composeSpec f g x = f (g x)

flipSpec :: (a -> b -> c) -> b -> a -> c
flipSpec f x y = f y x

maybeSpec :: b -> (a -> b) -> Maybe a -> b
maybeSpec d f z
  = case z of { Nothing -> d
              ; Just z  -> f z
              }

buildSpec :: forall a. (forall b. (a -> b -> b) -> b -> b) -> [a]
buildSpec g = g (:) []
