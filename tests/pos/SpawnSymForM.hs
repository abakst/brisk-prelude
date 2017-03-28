{- OPTIONS_GHC -fplugin Brisk.Plugin #-}
{- OPTIONS_GHC -fplugin-opt Brisk.Plugin:main #-}
{-# LANGUAGE TemplateHaskell #-}
module SpawnSym where

import Control.Monad (forM, foldM)
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess

import GHC.Base.Brisk

p :: ProcessId -> Process ()
p who = do self <- getSelfPid
           c <- liftIO $ getChar
           msg <- if c == 'x' then return (0 :: Int) else return 1
           send who msg
           expect :: Process ()
           return ()

remotable ['p]

ack :: ProcessId -> Process ()
ack p = send p ()

req = expect :: Process Int  

broadCast :: SymSet ProcessId -> Process ()
broadCast pids = do
  forM pids $ \p -> req
  forM pids $ \p -> ack p
  return ()

main :: [NodeId] -> Process ()
main nodes = do me     <- getSelfPid
                symSet <- spawnSymmetric nodes $ $(mkBriskClosure 'p) me
                broadCast symSet
                return ()
