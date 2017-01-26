{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Brisk.Plugin #-}
module Control.Distributed.Process.ManagedProcess.Brisk
  ( handleCallSpec
  , replySpec
  , callSpec
  , defaultProcessSpec
  , serveSpec
  ) where
import Brisk.Annotations as Annot
import Control.Distributed.BriskStatic.Internal
import Control.Distributed.Process hiding (call)
import Control.Distributed.Process.Serializable
import Control.Distributed.Process.ManagedProcess
  ( handleCall
  , serve
  , reply
  , defaultProcess
  , UnhandledMessagePolicy(..)
  , ProcessDefinition(..)
  , InitHandler(..)
  , InitResult(..)
  )
import Control.Distributed.Process.ManagedProcess.Server
import Control.Distributed.Process.ManagedProcess.Client
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
         ReplyMsg r s' -> do send who r
                             return s'

{-# ANN replySpec (Assume 'reply) #-}
replySpec :: forall r s.
             Serializable r
          => r -> s -> Process (ProcessReply r s)
replySpec msg st = return (ReplyMsg msg st)

{-# ANN defaultProcessSpec (Assume 'defaultProcess) #-}
defaultProcessSpec :: ProcessDefinition s
defaultProcessSpec
  = ProcessDefinition { apiHandlers            = []
                      , infoHandlers           = []
                      , exitHandlers           = []
                      , timeoutHandler         = Annot.top
                      , shutdownHandler        = Annot.top
                      , unhandledMessagePolicy = Terminate
                      }

{-# ANN serveSpec (Assume 'serve) #-}
serveSpec :: forall a s. a -> InitHandler a s -> ProcessDefinition s -> Process ()
serveSpec x ih (ProcessDefinition { apiHandlers = [h] })
  = do InitOk s0 d0 <- ih x
       recvLoop s0
         where
           recvLoop :: s -> Process ()
           recvLoop st0 = do s' <- (top `castEffect` h :: s -> Process s) st0
                             recvLoop s'

{-# ANN callSpec (Assume 'call) #-}
callSpec :: forall s a b.
            (Serializable a, Serializable b)
         => ProcessId -> a -> Process b
callSpec p m
  = do self <- getSelfPid
       send p (self, m)
       expect
