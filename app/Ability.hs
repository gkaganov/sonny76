{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Ability where

data Ability
  = Slash
  | Hack
  deriving (Eq)

instance Show Ability where
  show Slash = "slash"
  show Hack = "hack"

focusCost :: Ability -> Integer
focusCost Slash = 0
focusCost Hack = 40
