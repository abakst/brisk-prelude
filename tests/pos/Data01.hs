{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Scratch where
import Control.Distributed.Process
import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import GHC.Base.Brisk

data PingMessage = Ping ProcessId | Pong ProcessId
               deriving (Typeable, Generic)

instance Binary PingMessage

pingProcess :: ProcessId -> Process ()
pingProcess whom = do me <- getSelfPid
                      doSend <- liftIO $ getChar
                      if doSend == 'x' then
                        send whom (Ping me)
                      else
                        send whom (Ping me)
                      expect :: Process PingMessage
                      return ()
remotable ['pingProcess]

pongProcess :: Process ()
pongProcess = do msg <- expect
                 me  <- getSelfPid
                 case msg of
                   Ping whom -> send whom $ Pong me
                   Pong x    -> return ()

main :: NodeId -> Process ()
main n = do me  <- getSelfPid
            spawn n $ $(mkBriskClosure 'pingProcess) me
            pongProcess
