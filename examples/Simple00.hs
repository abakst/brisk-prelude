{-# LANGUAGE DeriveGeneric #-}
{- OPTIONS_GHC -fplugin Brisk.Plugin #-}
{- OPTIONS_GHC
    -fplugin-opt Brisk.Plugin:main
#-}
module Simple00 (foop) where

import GHC.Base.Brisk
import Control.Distributed.Process.Brisk
import Brisk.Annotations
import Brisk.Model.Types hiding (Process)
import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)

-- data Ping = Ping ProcessId | Pong ProcessId
--                deriving (Typeable, Generic)
-- instance Binary Ping

-- data ABigRecord = Foo { a :: Int, b :: ProcessId }         
--                deriving (Typeable, Generic)
-- instance Binary ABigRecord

foop :: Process () 
foop = do p       <- getSelfPid
          send p (Just ())
          -- let myBigRecord = Foo 3 p
          -- Just _  <- expect :: Process (Maybe Int)
          -- flip send (b myBigRecord) q
          -- msg <- expect
          -- send (b msg) ()
          return ()
