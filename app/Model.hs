{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Model where

import Ability
import Buff
import Hero

data Model =
  Model
    { heroesInBattle :: [Hero]
    , battleWinner :: Maybe BattleSide
    , humanActive :: Bool
    , hoveredElement :: Maybe Buff
    }
  deriving (Show, Eq)

data Action
  = NoOp
  | Print String
  | AbilityBtnPressed Int
  | AttackAnimationEnd HeroID
  | ElementHovered (Maybe Buff)
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
    , hoveredElement = Nothing
    }

newtype GuiEvent =
  AttackAnimationEndEvent HeroID
