{- OPTIONS_GHC -fplugin Brisk.Plugin #-}
{- OPTIONS_GHC -fplugin-opt Brisk.Plugin:main #-}
{-# LANGUAGE TemplateHaskell #-}
module SpawnSym (main) where

import Control.Monad (forM, foldM)
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess

import GHC.Base.Brisk

p :: ProcessId -> Process ()
p who = do self <- getSelfPid
           expect :: Process ()
           -- return ()

remotable ['p]

ack :: ProcessId -> Process ()
ack p = send p ()

broadCast :: SymSet ProcessId -> Process ()
broadCast pids
  = do foldM go () pids
       return ()
         where
           go _ = ack

main :: [NodeId] -> Process ()
main nodes = do me     <- getSelfPid
                symSet <- spawnSymmetric nodes $ $(mkBriskClosure 'p) me
                broadCast symSet
                return ()
