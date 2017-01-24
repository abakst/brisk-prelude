{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Brisk.Plugin #-}
module GHC.CString.Brisk (unpackCStringSpec) where
import GHC.Prim
import GHC.CString (unpackCString#)
import Brisk.Annotations
{-# ANN module SpecModule #-}

{-# ANN unpackCStringSpec (Assume 'unpackCString#) #-}
unpackCStringSpec :: Addr# -> Int
unpackCStringSpec _ = 0
