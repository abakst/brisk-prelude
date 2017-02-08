{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fplugin Brisk.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt Brisk.Plugin:main #-}
module Simple03 where
import Control.Monad
import Control.Distributed.Process
import Control.Distributed.Process.SymmetricProcess
import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)
import GHC.Base.Brisk


main :: SymSet ProcessId -> Process Int
main set
  = do me <- getSelfPid
       res <- foldM go 0 set
       return res
         where
           go :: Int -> ProcessId -> Process Int
           go acc x = do send x ()
                         msg <- expect :: Process Int
                         case msg of
                           0 -> return (1 + acc)
                           _ -> return (2 + acc)
