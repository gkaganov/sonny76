module AbilityImpl
  ( cast
  ) where

import Ability
import Buff
import Hero
import BuffImpl

import Data.Function

cast :: Ability -> HeroID -> HeroID -> [Hero] -> [Hero]
cast Slash caster target heroes =
  let damage = 150
      fCost = focusCost Slash
      buffs = [Buff {buffType = PeaceOfTheLightBlade, buffDuration = 2}]
   in defaultCast damage fCost buffs caster target heroes
cast Hack caster target heroes =
  let damage = 350
      fCost = focusCost Hack
      buffs = [Buff {buffType = PoisonOfTheHeavyBlade, buffDuration = 3}]
   in defaultCast damage fCost buffs caster target heroes
cast Destroy caster target heroes =
  let damage = 10000
      fCost = focusCost Destroy
      buffs = []
   in defaultCast damage fCost buffs caster target heroes
cast Wound caster target heroes =
  let damage = 100
      fCost = focusCost Wound
      buffs = []
   in defaultCast damage fCost buffs caster target heroes

defaultCast ::
     Integer -> Integer -> [Buff] -> HeroID -> HeroID -> [Hero] -> [Hero]
defaultCast damage fCost buffs caster target heroes =
  heroes & applyFocusLoss fCost caster & applyDamage damage target &
  applyBuffs buffs target
