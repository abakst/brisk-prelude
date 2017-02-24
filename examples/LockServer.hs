{-# OPTIONS_GHC -fplugin Brisk.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt Brisk.Plugin:scenario #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module SpawnSym (scenario) where

import           Data.Binary
import           GHC.Generics (Generic)
import           Control.Monad (forM, foldM)
import           Control.Distributed.Process hiding (call)
import           Control.Distributed.BriskStatic
import           Control.Distributed.Process.Closure
import           Control.Distributed.Process.SymmetricProcess
import           Control.Distributed.Process.Extras.Time
import           Control.Distributed.Process.ManagedProcess

import           GHC.Base.Brisk
import           Control.Distributed.Process.Brisk
import           Control.Exception.Base.Brisk
import           Control.Distributed.Process.ManagedProcess.Brisk

data Lock   = Lock ProcessId
            deriving (Generic)
data Grant  = Granted
            deriving (Generic)
data Release = Release 
            deriving (Generic)

instance Binary Lock
instance Binary Grant
instance Binary Release

client :: ProcessId -> Process ()
client server = do
  self    <- getSelfPid
  -- Request the lock
  send server (Lock self)
  Granted <- expect
  -- Do some work...

  -- ...Crickets...
  
  -- Now release the lock
  rel   <- selfSign Release
  send server rel


lockServer :: SymSet ProcessId -> Process ()
lockServer ps
  = foldM go () ps
  where
    go _ _ = do
       (Lock p) <- expect
       send p Granted
       Release <- expectFrom p
       return ()
       

remotable ['client]

scenario :: NodeId -> [NodeId] -> Process ()
scenario mnode cnodes
  = do master <- getSelfPid
       cs <- spawnSymmetric cnodes $ $(mkBriskClosure 'client) master
       lockServer cs
       return ()
