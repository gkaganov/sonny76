module SharedModel
  ( GameDescription(..)
  ) where

data GameDescription =
  GameDescription
    { descTitle :: String
    , descText :: String
    }
  deriving (Show, Eq)
