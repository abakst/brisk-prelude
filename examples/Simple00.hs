{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fplugin Brisk.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt Brisk.Plugin:main #-}
module Simple00 (main) where

import Control.Distributed.Process
import Brisk.Annotations
import Brisk.Model.Types hiding (Process)
import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)

import GHC.Base.Brisk
import GHC.CString.Brisk
import Control.Exception.Base.Brisk

data Ping = Ping ProcessId | Pong ProcessId
               deriving (Typeable, Generic)
instance Binary Ping

data ABigRecord = Foo { a :: Int, b :: ProcessId }         
               deriving (Typeable, Generic)
instance Binary ABigRecord

main :: Process () 
main = do p       <- getSelfPid
          let myBigRecord = Foo 3 p
          Ping q  <- expect
          flip send (b myBigRecord) q
          msg <- expect
          send (b msg) ()
          return ()
