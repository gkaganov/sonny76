{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Model where

import Ability
import Buff
import Hero
import SharedModel

data Model =
  Model
    { heroesInBattle :: [Hero]
    , battleWinner :: Maybe BattleSide
    , humanActive :: Bool
    , infoBoxContents :: GameDescription
    }
  deriving (Show, Eq)

data Action
  = NoOp
  | Print String
  | AbilityBtnPressed Int
  | AttackAnimationEnd HeroID
  | BuffHovered Buff
  | AbilityHovered Ability
  | NothingHovered
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
            , availableAbilities = [Slash, Hack, Wound, Destroy]
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
    , infoBoxContents = GameDescription {descTitle = "", descText = ""}
    }

newtype GuiEvent =
  AttackAnimationEndEvent HeroID
