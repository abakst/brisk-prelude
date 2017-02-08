{-# LANGUAGE TemplateHaskell #-}
module Control.Exception.Base.Brisk
       ( patErrorSpec
       ) where
import Control.Exception.Base
import Brisk.Annotations
{-# ANN module SpecModule #-}

{-# ANN patErrorSpec (Assume 'patError) #-}
patErrorSpec :: String -> a
patErrorSpec s = error s
