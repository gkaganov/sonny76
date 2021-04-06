module Model
  ( Model(..)
  , Hero(..)
  , HeroID
  , HeroAnimation
  , BattleSide(..)
  ) where

import Data.Sequence

data Model =
  Model
    { heroes :: Seq Hero
    , battleFinished :: Bool
    , humanActive :: Bool
    }
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
  deriving (Eq)

instance Show BattleSide where
  show LeftSide = "left"
  show RightSide = "right"
