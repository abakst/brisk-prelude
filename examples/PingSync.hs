{-# OPTIONS_GHC -fplugin Brisk.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt Brisk.Plugin:main #-}
{-# LANGUAGE TemplateHaskell #-}
module SpawnSym (main) where

import Control.Monad (forM, foldM)
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess

import GHC.Base.Brisk

p :: ProcessId -> Process ()
p whom = do self <- getSelfPid
            who  <- expect :: Process ProcessId
            msg  <- selfSign self
            send whom msg
            return ()

remotable ['p]

ack :: ProcessId -> Process ()
ack p = send p ()

pingPong :: SymSet ProcessId -> Process ()
pingPong pids
  = foldM go () pids
  where
    go _ x = do me  <- getSelfPid
                send x me
                expectFrom x :: Process ProcessId
                return ()

main :: [NodeId] -> Process ()
main nodes = do me     <- getSelfPid
                symSet <- spawnSymmetric nodes $ $(mkBriskClosure 'p) me
                pingPong symSet
