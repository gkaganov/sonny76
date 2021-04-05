module Model
  ( HeroType(..)
  , Hero(..)
  , Model(..)
  ) where

data Model =
  Model
    { player :: Hero
    , enemy :: Hero
    , battleFinished :: Bool
    , playerActive :: Bool
    }
  deriving (Show, Eq)

data HeroType
  = Player
  | Enemy
  deriving (Show, Eq)

data Hero =
  Hero
    { name :: String
    , health :: Integer
    , focusAmount :: Integer
    , slashing :: Bool
    , hacking :: Bool
    , dead :: Bool
    }
  deriving (Show, Eq)
