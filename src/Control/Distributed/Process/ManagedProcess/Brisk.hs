{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Control.Distributed.Process.ManagedProcess.Brisk
  ( handleCallSpec
  , replySpec
  , callSpec
  , defaultProcessSpec
  , serveSpec
  , module Control.Distributed.Process.ManagedProcess
  ) where
import Brisk.Annotations as Annot
import Control.Distributed.BriskStatic.Internal
import Control.Distributed.Process hiding (call)
import Control.Distributed.Process.Serializable
import Control.Distributed.Process.SymmetricProcess
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
data ProcessReply b s = ReplyMsg b s | NoReply  s
type Dispatcher s     = s -> Process s

{-# ANN handleCallSpec (Assume 'handleCall) #-}
handleCallSpec :: forall a b s.
                  (Serializable a, Serializable b)
               => (s -> a -> Process (ProcessReply b s))
               -> Dispatcher s
handleCallSpec f s0
  = do (who,msg) <- expect :: Process (ProcessId, a)
       reply     <- f s0 msg
       case reply of
         NoReply s'    -> return s'
         ReplyMsg r s' -> do resp <- selfSign r
                             send who resp
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
serveSpec :: forall goo st. goo -> InitHandler goo st -> ProcessDefinition st -> Process ()
serveSpec x ih (ProcessDefinition { apiHandlers = [h] })
  = do InitOk s0 d0 <- ih x
       recvLoop s0
         where
           recvLoop :: st -> Process ()
           recvLoop st0 = do s' <- (top `castEffect` h :: st -> Process st) st0
                             recvLoop s'

{-# ANN callSpec (Assume 'call) #-}
callSpec :: forall s a b.
            (Serializable a, Serializable b)
         => ProcessId -> a -> Process b
callSpec p m
  = do self <- getSelfPid
       send p m
       expectFrom p
