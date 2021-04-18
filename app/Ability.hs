{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Ability
  ( Ability(..)
  , focusCost
  , description
  , numbers
  ) where

import Buff hiding (description, numbers)
import SharedModel

data Ability
  = Slash
  | Hack
  | Wound
  | Destroy
  deriving (Eq)

instance Show Ability where
  show Slash = "Slash"
  show Hack = "Hack"
  show Wound = "Wound"
  show Destroy = "Destroy"

focusCost :: Ability -> Integer
focusCost Slash = 0
focusCost Hack = 40
focusCost Destroy = 60
focusCost Wound = 0

numbers :: Ability -> [Integer]
numbers Slash = [150]
numbers Hack = [350]
numbers Wound = [100]
numbers Destroy = [10000]

description :: Ability -> GameDescription
description Slash =
  GameDescription
    { descTitle = show Slash
    , descText =
        "Slash the enemy for " ++
        show (head $ numbers Slash) ++
        " damage and apply " ++ show PeaceOfTheLightBlade
    }
description Hack =
  GameDescription
    { descTitle = show Hack
    , descText =
        "Hack the enemy for " ++
        show (head $ numbers Hack) ++
        " damage and apply " ++ show PoisonOfTheHeavyBlade
    }
description Wound =
  GameDescription
    { descTitle = show Wound
    , descText =
        "Attack the enemy for " ++ show (head $ numbers Wound) ++ " damage"
    }
description Destroy =
  GameDescription {descTitle = show Destroy, descText = "Destroy the enemy"}
