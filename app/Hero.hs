{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Hero where

import Ability

import Data.Foldable (find)
import Data.Function ((&))
import Data.Sequence (Seq)

import qualified Data.Sequence as Seq

data Hero =
  Hero
    { heroID :: HeroID
    , name :: String
    , battleSide :: BattleSide
    , health :: Integer
    , focusAmount :: Integer
    , currentAnimation :: HeroAnimation
    , dead :: Bool
    }
  deriving (Show, Eq)

type HeroID = Integer

data HeroAnimation
  = Idling
  | Slashing
  | Hacking
  deriving (Eq, Show)

data BattleSide
  = LeftSide
  | RightSide
  deriving (Show, Eq)

canCast :: Ability -> HeroID -> Seq Hero -> Bool
canCast = enoughFocus

enoughFocus :: Ability -> HeroID -> Seq Hero -> Bool
enoughFocus a hID m =
  let hero = findHero hID m
      currentFocus = focusAmount hero
   in currentFocus >= focusCost a

applyDamage :: Integer -> HeroID -> Seq Hero -> Seq Hero
applyDamage damage targetID m =
  let hero = findHero targetID m
   in adjustHealth hero (-damage) & updateHero m

setHeroAnimation :: HeroAnimation -> HeroID -> Seq Hero -> Seq Hero
setHeroAnimation anim hID m =
  findHero hID m & (\h -> h {currentAnimation = anim}) & updateHero m

updateHero :: Seq Hero -> Hero -> Seq Hero
updateHero heroes newHero =
  case Seq.findIndexL (\h -> heroID h == heroID newHero) heroes of
    Just i -> Seq.update i newHero heroes
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

applyFocusCost :: Integer -> HeroID -> Seq Hero -> Seq Hero
applyFocusCost cost targetID m =
  let target = findHero targetID m
   in adjustFocus (-cost) target & updateHero m

adjustFocus :: Integer -> Hero -> Hero
adjustFocus value hero = hero {focusAmount = focusAmount hero + value}

findHeroID :: BattleSide -> Seq Hero -> HeroID
findHeroID side heroes =
  case find (\h -> battleSide h == side) heroes of
    Just hero -> heroID hero
    Nothing ->
      error $
      "i tried to find a hero on the " ++ show side ++ " battle side and failed"

findHero :: HeroID -> Seq Hero -> Hero
findHero hID heroes =
  case find (\h -> heroID h == hID) heroes of
    Just hero -> hero
    Nothing ->
      error $ "i tried to find a hero by id " ++ show hID ++ " and failed"

handleAbilityAnimation :: Ability -> HeroID -> Seq Hero -> Seq Hero
handleAbilityAnimation Slash = setHeroAnimation Slashing
handleAbilityAnimation Hack = setHeroAnimation Hacking
