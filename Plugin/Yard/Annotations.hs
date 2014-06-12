{-# LANGUAGE DeriveDataTypeable #-}
module Plugin.Yard.Annotations where

import Data.Data

data Optimize = DontOptimize deriving (Data, Typeable, Eq)