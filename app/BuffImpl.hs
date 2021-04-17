{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module BuffImpl
  ( triggerAllBuffs
  ) where

import Buff
import Hero

import Data.Function ((&))

triggerAllBuffs :: HeroID -> [Hero] -> [Hero]
triggerAllBuffs hID heroes =
  let hero = findHero hID heroes
   in triggerBuffs (activeBuffs hero) hID heroes

triggerBuffs :: [Buff] -> HeroID -> [Hero] -> [Hero]
triggerBuffs [buff] hID heroes = triggerBuff buff hID heroes
triggerBuffs (buff:buffs) hID heroes =
  triggerBuffs [buff] hID heroes & triggerBuffs buffs hID
triggerBuffs [] _ heroes = heroes

triggerBuff :: Buff -> HeroID -> [Hero] -> [Hero]
triggerBuff PoisonOfTheHeavyBlade target heroes = applyDamage 100 target heroes
triggerBuff PeaceOfTheLightBlade target heroes =
  applyFocusLoss 100 target heroes
