{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fplugin Brisk.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt Brisk.Plugin:handleCallSpec #-}
{-# OPTIONS_GHC -fplugin-opt Brisk.Plugin:replySpec #-}
{-# LANGUAGE TemplateHaskell #-}
module Control.Distributed.Process.ManagedProcess.Server.Brisk
  ( handleCallSpec
  , replySpec
  ) where
import Brisk.Annotations
import Control.Distributed.Process
import Control.Distributed.Process.Serializable
import Control.Distributed.Process.ManagedProcess
  (handleCall, reply)
{-# ANN module SpecModule #-}
-- 'Emebed' the ProcessReply and Dispatcher types
-- as the following:
data ProcessReply b s = ReplyMsg b s
                      | NoReply  s
type Dispatcher s     = s -> Process s

{-# ANN handleCallSpec (Assume 'handleCall) #-}
handleCallSpec :: forall s a b.
                  (Serializable a, Serializable b)
               => (s -> a -> Process (ProcessReply b s))
               -> Dispatcher s
handleCallSpec f s0
  = do (who,msg) <- expect :: Process (ProcessId, a)
       reply     <- f s0 msg
       case reply of
         NoReply s'    -> return s'
         ReplyMsg r s' -> send who r >> return s'

{-# ANN replySpec (Assume 'reply) #-}
replySpec :: forall r s.
             Serializable r
          => r -> s -> Process (ProcessReply r s)
replySpec msg st = return (ReplyMsg msg st)
