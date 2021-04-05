module Abilities
  ( Ability(..)
  , canCast
  , cast
  ) where

import Data.Function
import Model

focusCost :: Ability -> Integer
focusCost Slash = 0
focusCost Hack = 40

data Ability
  = Slash
  | Hack
  deriving (Eq)

instance Show Ability where
  show Slash = "slash"
  show Hack = "hack"

cast :: Ability -> HeroType -> HeroType -> Model -> Model
cast Slash _ target m =
  let damage = 150
   in m & applyDamage damage target
cast Hack caster target m =
  let damage = 350
      fCost = focusCost Hack
   in m & applyFocusCost fCost caster & applyDamage damage target

canCast :: Ability -> Model -> HeroType -> Bool
canCast Slash _ _ = True
canCast Hack m h =
  let fCost = focusCost Hack
   in case h of
        Player -> enoughFocus m Player Hack fCost
        Enemy -> False

enoughFocus :: Model -> HeroType -> Ability -> Integer -> Bool
enoughFocus m _ _ reqFocus =
  let currentFocus = m & player & focusAmount
   in currentFocus >= reqFocus

applyDamage :: Integer -> HeroType -> Model -> Model
applyDamage damage' target m =
  case target of
    Player -> m {player = adjustHealth (player m) (-damage')}
    Enemy -> m {enemy = adjustHealth (enemy m) (-damage')}

adjustHealth :: Hero -> Integer -> Hero
adjustHealth hero value =
  hero {health = health hero + value} & determineIfHeroKilled

determineIfHeroKilled :: Hero -> Hero
determineIfHeroKilled hero =
  if health hero <= 0
    then hero {dead = True, health = 0}
    else hero

applyFocusCost :: Integer -> HeroType -> Model -> Model
applyFocusCost cost target m =
  case target of
    Player -> m {player = adjustFocus (player m) (-cost)}
    Enemy -> m {enemy = adjustFocus (enemy m) (-cost)}

adjustFocus :: Hero -> Integer -> Hero
adjustFocus hero cost = hero {focusAmount = focusAmount hero + cost}
