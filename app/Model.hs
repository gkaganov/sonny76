{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Model where

import qualified Data.Sequence as Seq

data Model =
  Model
    { heroes :: Seq.Seq Hero
    , battleFinished :: Bool
    , humanActive :: Bool
    }
  deriving (Show, Eq)

initialModel :: Model
initialModel =
  Model
    { heroes =
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

data Action
  = NoOp
  | Print String
  | AbilityBtnPressed Integer
  | AttackAnimationEnd HeroID
  | Restart
  deriving (Show, Eq)

data Hero =
  Hero
    { heroID :: HeroID
    , name :: String
    , battleSide :: BattleSide
    , health :: Integer
    , focusAmount :: Integer
    , currentAnimation :: HeroAnimation
    , dead :: Bool
    }
  deriving (Show, Eq)

type HeroID = Integer

data HeroAnimation
  = Idling
  | Slashing
  | Hacking
  deriving (Eq, Show)

data BattleSide
  = LeftSide
  | RightSide
  deriving (Show, Eq)
