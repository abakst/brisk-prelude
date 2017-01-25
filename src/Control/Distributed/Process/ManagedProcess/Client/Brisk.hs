{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fplugin Brisk.Plugin #-}
{-# LANGUAGE TemplateHaskell #-}
module Control.Distributed.Process.ManagedProcess.Client.Brisk
  ( callSpec
  ) where
import Brisk.Annotations
import Control.Distributed.Process hiding (call)
import Control.Distributed.Process.Serializable
import Control.Distributed.Process.ManagedProcess.Client
import Control.Distributed.Process.ManagedProcess.Server
import Control.Distributed.Process.ManagedProcess.Server.Brisk
{-# ANN module SpecModule #-}
{-# ANN callSpec (Assume 'call) #-}
callSpec :: forall s a b.
            (Serializable a, Serializable b)
         => ProcessId -> a -> Process b
callSpec p m
  = do self <- getSelfPid
       send p (self, m)
       expect
