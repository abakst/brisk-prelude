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
p whom = do self <- getSelfPid
            who  <- expect :: Process ProcessId
            msg  <- selfSign self
            send who msg
            return ()
remotable ['p]

pingPong :: ProcessId -> Process ()
pingPong pid
  = do me  <- getSelfPid
       send pid me
       them <- expectFrom pid :: Process ProcessId
       return ()

main :: NodeId -> Process ()
main node = do me   <- getSelfPid
               them <- spawn node $ $(mkBriskClosure 'p) me
               pingPong them
