{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Buff where

import SharedModel

data Buff =
  Buff
    { buffType :: BuffType
    , buffDuration :: Int
    }
  deriving (Show, Eq)

data BuffType
  = PoisonOfTheHeavyBlade
  | PeaceOfTheLightBlade
  deriving (Eq)

instance Show BuffType where
  show PoisonOfTheHeavyBlade = "Poison of the Heavy Blade"
  show PeaceOfTheLightBlade = "Peace of the Light Blade"

numbers :: BuffType -> [Integer]
numbers PoisonOfTheHeavyBlade = [100]
numbers PeaceOfTheLightBlade = [50]

description :: Buff -> GameDescription
description buff =
  case buffType buff of
    PoisonOfTheHeavyBlade ->
      GameDescription
        { descTitle = show PoisonOfTheHeavyBlade
        , descText =
            "This hero is poisoned and takes " ++
            show (head $ numbers PoisonOfTheHeavyBlade) ++
            " damage every turn for " ++ turns (buffDuration buff)
        }
    PeaceOfTheLightBlade ->
      GameDescription
        { descTitle = show PeaceOfTheLightBlade
        , descText =
            "This hero is pacified and loses " ++
            show (head $ numbers PeaceOfTheLightBlade) ++
            " focus every turn for " ++ turns (buffDuration buff)
        }

turns :: Int -> String
turns duration =
  show duration ++
  if duration == 1
    then " turn"
    else " turns"
