{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game.Horca.Types where

import           Lens.Micro.TH
import           Data.Text

import Data.IORef

newtype BPM = BPM Integer
  deriving (Eq, Ord, Show, Read, Num)

data Tick = Tick deriving (Show)
