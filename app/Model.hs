{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Model where

import Ability
import Hero

data Model =
  Model
    { heroesInBattle :: [Hero]
    , battleWinner :: Maybe BattleSide
    , humanActive :: Bool
    }
  deriving (Show, Eq)

data Action
  = NoOp
  | Print String
  | AbilityBtnPressed Int
  | AttackAnimationEnd HeroID
  | Restart
  deriving (Show, Eq)

initialModel :: Model
initialModel =
  Model
    { heroesInBattle =
        [ Hero
            { name = "bro"
            , heroID = 0
            , battleSide = LeftSide
            , health = 1000
            , focusAmount = 100
            , activeBuffs = []
            , availableAbilities = [Slash, Hack]
            , currentAnimation = Idling
            , activeStatusEffects = []
            , dead = False
            }
        , Hero
            { name = "the baddies"
            , heroID = 1
            , battleSide = RightSide
            , health = 1000
            , focusAmount = 100
            , activeBuffs = []
            , availableAbilities = [Wound, Destroy]
            , currentAnimation = Idling
            , activeStatusEffects = []
            , dead = False
            }
        ]
    , battleWinner = Nothing
    , humanActive = True
    }

newtype GuiEvent =
  AttackAnimationEndEvent HeroID
