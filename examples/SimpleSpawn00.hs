{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fplugin Brisk.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt Brisk.Plugin:main #-}
module Simple00 (main) where

import GHC.Base.Brisk
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Brisk
import Brisk.Annotations
import Brisk.Model.Types hiding (Process)
import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)

data Ping = Ping ProcessId | Pong ProcessId
               deriving (Typeable, Generic)
instance Binary Ping

data ABigRecord = Foo { a :: Int, b :: ProcessId }         
               deriving (Typeable, Generic)
instance Binary ABigRecord

child :: ProcessId -> Process ()
child p = do me <- getSelfPid
             send p (Ping me)
             msg <- expect
             case msg of
               Foo { b = q } -> send q (Foo { a = 0, b = me })
             expect :: Process ()

remotable ['child]

main :: NodeId -> Process () 
main n =
  do p       <- getSelfPid
     q       <- spawn n $ $(mkBriskClosure 'child) p
     let myBigRecord = Foo 3 p
     Ping q  <- expect
     flip send (b myBigRecord) q
     msg <- expect
     send (b msg) ()
     return ()
