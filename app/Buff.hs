{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Buff where

data Buff
  = Poisoned
  | Silenced
  deriving (Show, Eq)
