{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Control.Distributed.Process.Brisk
  ( saySpec
  , regSpec
  , getSelfNodeSpec
  , module Control.Distributed.Process
  ) where
import Brisk.Annotations
import Control.Distributed.Process
{-# ANN module SpecModule #-}

{-# ANN saySpec (Assume 'say) #-}
saySpec :: String -> Process ()
saySpec _ = return ()

{-# ANN regSpec (Assume 'register) #-}
regSpec :: String -> ProcessId -> Process ()
regSpec _ _ = return ()

{-# ANN getSelfNodeSpec (Assume 'getSelfNode) #-}
getSelfNodeSpec :: Process NodeId
getSelfNodeSpec = return top
