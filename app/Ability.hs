{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Ability where

data Ability
  = Slash
  | Hack
  deriving (Show, Eq)

focusCost :: Ability -> Integer
focusCost Slash = 5
focusCost Hack = 40
