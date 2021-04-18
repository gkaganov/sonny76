{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Hero where

import Ability
import Buff

import Data.Foldable (find, toList)
import Data.Function ((&))

import qualified Data.Sequence as Seq

data Hero =
  Hero
    { heroID :: HeroID
    , name :: String
    , battleSide :: BattleSide
    , health :: Integer
    , focusAmount :: Integer
    , activeBuffs :: [Buff]
    , activeStatusEffects :: [StatusEffect]
    , availableAbilities :: [Ability]
    , currentAnimation :: HeroAnimation
    , dead :: Bool
    }
  deriving (Show, Eq)

type HeroID = Integer

data StatusEffect =
  Silenced
  deriving (Eq, Show)

data HeroAnimation
  = Idling
  | Slashing
  | Hacking
  deriving (Eq, Show)

data BattleSide
  = LeftSide
  | RightSide
  deriving (Show, Eq)

canCast :: Ability -> HeroID -> [Hero] -> Bool
canCast = enoughFocus

enoughFocus :: Ability -> HeroID -> [Hero] -> Bool
enoughFocus a hID heroes =
  let hero = findHero hID heroes
      currentFocus = focusAmount hero
   in currentFocus >= focusCost a

applyDamage :: Integer -> HeroID -> [Hero] -> [Hero]
applyDamage damage targetID heroes =
  let hero = findHero targetID heroes
   in adjustHealth hero (-damage) & updateHero heroes

setHeroAnimation :: HeroAnimation -> HeroID -> [Hero] -> [Hero]
setHeroAnimation anim hID heroes =
  findHero hID heroes & (\h -> h {currentAnimation = anim}) & updateHero heroes

updateHero :: [Hero] -> Hero -> [Hero]
updateHero arrayHeroes newHero =
  let heroes = Seq.fromList arrayHeroes
   in case Seq.findIndexL (\h -> heroID h == heroID newHero) heroes of
        Just i -> Seq.update i newHero heroes & toList
        Nothing ->
          error $
          "i tried to update a hero by id " ++
          show (heroID newHero) ++ " and failed"

adjustHealth :: Hero -> Integer -> Hero
adjustHealth hero value =
  hero {health = health hero + value} & determineIfHeroKilled

determineIfHeroKilled :: Hero -> Hero
determineIfHeroKilled hero =
  if health hero <= 0
    then hero {dead = True, health = 0}
    else hero

applyFocusLoss :: Integer -> HeroID -> [Hero] -> [Hero]
applyFocusLoss amount targetID heroes =
  let target = findHero targetID heroes
   in adjustFocus (-amount) target & updateHero heroes

adjustFocus :: Integer -> Hero -> Hero
adjustFocus value hero = hero {focusAmount = focusAmount hero + value}

findHeroID :: BattleSide -> [Hero] -> HeroID
findHeroID side heroes =
  case find (\h -> battleSide h == side) heroes of
    Just hero -> heroID hero
    Nothing ->
      error $
      "i tried to find a hero on the " ++ show side ++ " battle side and failed"

findHero :: HeroID -> [Hero] -> Hero
findHero hID heroes =
  case find (\h -> heroID h == hID) heroes of
    Just hero -> hero
    Nothing ->
      error $ "i tried to find a hero by id " ++ show hID ++ " and failed"

applyStatusEffect :: StatusEffect -> HeroID -> [Hero] -> [Hero]
applyStatusEffect effect hID heroes =
  let hero = findHero hID heroes
   in hero {activeStatusEffects = (hero & activeStatusEffects) ++ [effect]} &
      updateHero heroes

handleAbilityAnimation :: Ability -> HeroID -> [Hero] -> [Hero]
handleAbilityAnimation Slash = setHeroAnimation Slashing
handleAbilityAnimation Hack = setHeroAnimation Hacking
handleAbilityAnimation Destroy = setHeroAnimation Hacking
handleAbilityAnimation Wound = setHeroAnimation Slashing
