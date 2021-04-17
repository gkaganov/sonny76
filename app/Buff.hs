{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Buff where

data Buff
  = PoisonOfTheHeavyBlade
  | PeaceOfTheLightBlade
  deriving (Show, Eq)

data BuffDescription =
  BuffDescription
    { descTitle :: String
    , descText :: String
    }

description :: Buff -> BuffDescription
description PoisonOfTheHeavyBlade =
  BuffDescription
    { descTitle = "Poison of the Heavy Blade"
    , descText = "This hero is poisoned and takes 100 damage every turn"
    }
description PeaceOfTheLightBlade =
  BuffDescription
    { descTitle = "Peace of the Light Blade"
    , descText = "This hero is pacified and loses 100 focus every turn"
    }
