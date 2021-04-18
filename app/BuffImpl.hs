{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module BuffImpl
  ( triggerAllBuffs
  , tickBuffDuration
  , applyBuffs
  ) where

import Buff
import Hero

import Data.Function ((&))

triggerAllBuffs :: HeroID -> [Hero] -> [Hero]
triggerAllBuffs hID heroes =
  let hero = findHero hID heroes
   in triggerBuffs (activeBuffs hero) hID heroes

triggerBuffs :: [Buff] -> HeroID -> [Hero] -> [Hero]
triggerBuffs [buff] hID heroes = triggerBuff (buffType buff) hID heroes
triggerBuffs (buff:buffs) hID heroes =
  triggerBuffs [buff] hID heroes & triggerBuffs buffs hID
triggerBuffs [] _ heroes = heroes

triggerBuff :: BuffType -> HeroID -> [Hero] -> [Hero]
triggerBuff PoisonOfTheHeavyBlade target heroes =
  applyDamage (head $ numbers PoisonOfTheHeavyBlade) target heroes
triggerBuff PeaceOfTheLightBlade target heroes =
  applyFocusLoss (head $ numbers PeaceOfTheLightBlade) target heroes

applyBuffs :: [Buff] -> HeroID -> [Hero] -> [Hero]
applyBuffs buffs hID heroes =
  let hero = findHero hID heroes
   in addBuffs buffs hero & updateHero heroes

addBuffs :: [Buff] -> Hero -> Hero
addBuffs buffs hero = hero {activeBuffs = (hero & activeBuffs) ++ buffs}

tickBuffDuration :: HeroID -> [Hero] -> [Hero]
tickBuffDuration hID heroes =
  let hero = findHero hID heroes
   in hero
        { activeBuffs =
            map
              (\buff -> buff {buffDuration = buffDuration buff - 1})
              (activeBuffs hero) &
            filter (\buff -> buffDuration buff > 0)
        } &
      updateHero heroes
