module Abilities
  ( Ability(..)
  , slash
  , hack
  ) where

import Data.Function
import Types

data Ability =
  Ability
    { abilityName :: String
    , action :: HeroType -> HeroType -> Model -> Model
    }

slash :: Ability
slash =
  Ability
    { abilityName = "slash"
    , action =
        \_ target m ->
          let damage = 100
           in applyDamage damage target m
    }

hack :: Ability
hack =
  Ability
    { abilityName = "hack"
    , action =
        \caster target m ->
          let damage = 350
              focusCost = 40
           in m & applyFocusCost focusCost caster & applyDamage damage target
    }

applyDamage :: Integer -> HeroType -> Model -> Model
applyDamage damage target m =
  case target of
    Player -> m {player = adjustHealth (player m) (-damage)}
    Enemy -> m {enemy = adjustHealth (enemy m) (-damage)}

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
adjustFocus hero cost = hero {focusAmount = focusAmount hero - cost}
