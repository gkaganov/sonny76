{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Model where

import Hero

import Data.Sequence as Seq (Seq, fromList)

data Model =
  Model
    { heroesInBattle :: Seq Hero
    , battleFinished :: Bool
    , humanActive :: Bool
    }
  deriving (Show, Eq)

data Action
  = NoOp
  | Print String
  | AbilityBtnPressed Integer
  | AttackAnimationEnd HeroID
  | Restart
  deriving (Show, Eq)

initialModel :: Model
initialModel =
  Model
    { heroesInBattle =
        Seq.fromList
          [ Hero
              { name = "bro"
              , heroID = 0
              , battleSide = LeftSide
              , health = 1000
              , focusAmount = 100
              , currentAnimation = Idling
              , dead = False
              }
          , Hero
              { name = "the baddies"
              , heroID = 1
              , battleSide = RightSide
              , health = 1000
              , focusAmount = 100
              , currentAnimation = Idling
              , dead = False
              }
          ]
    , battleFinished = False
    , humanActive = True
    }

newtype GuiEvent =
  AttackAnimationEndEvent HeroID
