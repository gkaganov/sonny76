module AbilityImpl
  ( cast
  ) where

import Ability
import Buff hiding (numbers)
import BuffImpl
import Hero

import Data.Function

cast :: Ability -> HeroID -> HeroID -> [Hero] -> [Hero]
cast Slash caster target heroes =
  let damage = head $ numbers Slash
      fCost = focusCost Slash
      buffs = [Buff {buffType = PeaceOfTheLightBlade, buffDuration = 2}]
   in defaultCast damage fCost buffs caster target heroes
cast Hack caster target heroes =
  let damage = head $ numbers Hack
      fCost = focusCost Hack
      buffs = [Buff {buffType = PoisonOfTheHeavyBlade, buffDuration = 3}]
   in defaultCast damage fCost buffs caster target heroes
cast Wound caster target heroes =
  let damage = head $ numbers Wound
      fCost = focusCost Wound
      buffs = []
   in defaultCast damage fCost buffs caster target heroes
cast Destroy caster target heroes =
  let damage = head $ numbers Destroy
      fCost = focusCost Destroy
      buffs = []
   in defaultCast damage fCost buffs caster target heroes

defaultCast ::
     Integer -> Integer -> [Buff] -> HeroID -> HeroID -> [Hero] -> [Hero]
defaultCast damage fCost buffs caster target heroes =
  heroes & applyFocusLoss fCost caster & applyDamage damage target &
  applyBuffs buffs target
