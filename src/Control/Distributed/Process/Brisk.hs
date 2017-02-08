{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Control.Distributed.Process.Brisk
  ( saySpec
  , regSpec
  , module Control.Distributed.Process
  ) where
import Brisk.Annotations
import Control.Distributed.Process
import Control.Distributed.Process.Internal.Types
import Control.Distributed.Process.Internal.Primitives
{-# ANN module SpecModule #-}

{-# ANN saySpec (Assume 'say) #-}
saySpec :: String -> Process ()
saySpec _ = return ()

{-# ANN regSpec (Assume 'register) #-}
regSpec :: String -> ProcessId -> Process ()
regSpec _ _ = return ()
