module AbilityList
  ( cast
  ) where

import AbilityData
import Hero
import Model

import Data.Function

cast :: Ability -> HeroID -> HeroID -> Model -> Model
cast Slash _ target m =
  let damage = 150
   in m & applyDamage damage target
cast Hack caster target m =
  let damage = 350
      fCost = focusCost Hack
   in m & applyFocusCost fCost caster & applyDamage damage target
