{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module AbilityData where

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
