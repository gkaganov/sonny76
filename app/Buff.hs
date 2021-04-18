{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Buff where

data Buff =
  Buff
    { buffType :: BuffType
    , buffDuration :: Int
    }
  deriving (Show, Eq)

data BuffType
  = PoisonOfTheHeavyBlade
  | PeaceOfTheLightBlade
  deriving (Show, Eq)

data BuffDescription =
  BuffDescription
    { descTitle :: String
    , descText :: String
    }

numbers :: BuffType -> [Integer]
numbers PoisonOfTheHeavyBlade = [100]
numbers PeaceOfTheLightBlade = [50]

description :: Buff -> BuffDescription
description buff =
  case buffType buff of
    PoisonOfTheHeavyBlade ->
      BuffDescription
        { descTitle = "Poison of the Heavy Blade"
        , descText =
            "This hero is poisoned and takes " ++
            show (head $ numbers PoisonOfTheHeavyBlade) ++
            " damage every turn for " ++
            show (buffDuration buff) ++
            if buffDuration buff == 1
              then " turn"
              else " turns"
        }
    PeaceOfTheLightBlade ->
      BuffDescription
        { descTitle = "Peace of the Light Blade"
        , descText =
            "This hero is pacified and loses " ++
            show (head $ numbers PeaceOfTheLightBlade) ++
            " focus every turn for " ++
            show (buffDuration buff) ++
            if buffDuration buff == 1
              then " turn"
              else " turns"
        }
