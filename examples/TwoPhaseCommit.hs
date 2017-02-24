{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fplugin Brisk.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt Brisk.Plugin:main #-}
{-# LANGUAGE TemplateHaskell #-}
module SpawnSym where

import Control.Monad (forM, foldM)
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess
import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)

import GHC.Base.Brisk
import System.Directory

data AcceptorResponse = Accept ProcessId | Reject
                      deriving (Typeable, Generic)
instance Binary AcceptorResponse

data AcceptorAck = ACK
                      deriving (Typeable, Generic)
instance Binary AcceptorAck

data CoordMessage = Commit ProcessId | Rollback ProcessId
                      deriving (Typeable, Generic)
instance Binary CoordMessage

coordPid (Commit p)   = p      
coordPid (Rollback p) = p

acceptor :: Process ()
acceptor = do
  me            <- getSelfPid
  (who, fn)     <- expect :: Process (ProcessId, String)

  -- Do the transaction
  exists <- liftIO $ doesDirectoryExist fn
  if exists then
    send who (Accept me)
  else
    send who Reject

  -- Wait for message to commit
  msg <- expect :: Process CoordMessage

  send (coordPid msg) ACK

coord :: String -> SymSet ProcessId -> Process ()
coord f as = do foldM query () as
                n <- foldM countVotes 0 as
                if n == size as then
                  foldM doCommit () as
                else
                  foldM doAbort () as
  where
    doCommit () x  = do { me <- getSelfPid; send x (Commit me)   }
    doAbort  () x  = do { me <- getSelfPid; send x (Rollback me) }
    query () x     = do { me <- getSelfPid; send x (me, f)       }
    countVotes x y = do msg <- expect
                        case msg of
                          Accept _ -> return (x + 1)
                          Reject   -> return x


remotable ['acceptor]

main :: String -> [NodeId] -> Process ()
main fn nodes = do me <- getSelfPid
                   as <- spawnSymmetric nodes $ $(mkBriskStaticClosure 'acceptor)
                   coord fn as
