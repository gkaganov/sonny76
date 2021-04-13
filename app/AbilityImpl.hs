module AbilityImpl
  ( cast
  ) where

import Ability
import Hero

import Data.Function
import Data.Sequence (Seq)

cast :: Ability -> HeroID -> HeroID -> Seq Hero -> Seq Hero
cast Slash caster target heroes =
  let damage = 150
      fCost = focusCost Slash
   in defaultCast damage fCost caster target heroes
cast Hack caster target heroes =
  let damage = 350
      fCost = focusCost Hack
   in defaultCast damage fCost caster target heroes

defaultCast :: Integer -> Integer -> HeroID -> HeroID -> Seq Hero -> Seq Hero
defaultCast damage fCost caster target heroes =
  heroes & applyFocusCost fCost caster & applyDamage damage target
