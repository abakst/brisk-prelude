{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fplugin Brisk.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt Brisk.Plugin:main #-}
module Simple00 (main) where

import GHC.Base.Brisk
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

main :: Process () 
main = do p       <- getSelfPid
          let myBigRecord = Foo 3 p
          Ping q  <- expect
          flip send (b myBigRecord) q
          msg <- expect
          send (b msg) ()
          return ()
