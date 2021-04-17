{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Ability
  ( Ability(..)
  , focusCost
  ) where

data Ability
  = Slash
  | Hack
  | Wound
  | Destroy
  deriving (Show, Eq)

focusCost :: Ability -> Integer
focusCost Slash = 0
focusCost Hack = 40
focusCost Destroy = 60
focusCost Wound = 0
