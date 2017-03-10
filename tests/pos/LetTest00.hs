{- OPTIONS_GHC -fplugin Brisk.Plugin #-}
{- OPTIONS_GHC -fplugin-opt Brisk.Plugin:main #-}
{-# LANGUAGE TemplateHaskell #-}
module LetTest (main) where

import Control.Monad (forM, foldM)
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess

import GHC.Base.Brisk

p :: ProcessId -> Process ()
p whom = do le_me <- getSelfPid
            loop le_me
  where loop me = do expect :: Process ()
                     msg <- selfSign (0 :: Int)
                     send whom msg
                     loop me

remotable ['p]

ack :: ProcessId -> Process ()
ack p = send p ()

main :: [NodeId] -> Process ()
main nodes = do me     <- getSelfPid
                symSet <- spawnSymmetric nodes $ $(mkBriskClosure 'p) me
                let p0 = chooseSymmetric symSet 0
                ack p0
                expectFrom p0 :: Process Int
                return ()
