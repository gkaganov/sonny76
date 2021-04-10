{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Hero where

import Ability
import Model

import Data.Foldable (find)
import Data.Function ((&))
import qualified Data.Sequence as Seq

canCast :: Ability -> Model -> HeroID -> Bool
canCast Slash _ _ = True
canCast Hack m hID = enoughFocus hID Hack m

enoughFocus :: HeroID -> Ability -> Model -> Bool
enoughFocus hID a m =
  let hero = findHero hID m
      currentFocus = focusAmount hero
   in currentFocus >= focusCost a

applyDamage :: Integer -> HeroID -> Model -> Model
applyDamage damage targetID m =
  let hero = findHero targetID m
   in adjustHealth hero (-damage) & updateHero m

setHeroAnimation :: HeroAnimation -> HeroID -> Model -> Model
setHeroAnimation anim hID m =
  findHero hID m & (\h -> h {currentAnimation = anim}) & updateHero m

updateHero :: Model -> Hero -> Model
updateHero m newHero =
  case Seq.findIndexL (\h -> heroID h == heroID newHero) (m & heroes) of
    Just i -> m {heroes = Seq.update i newHero (m & heroes)}
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

applyFocusCost :: Integer -> HeroID -> Model -> Model
applyFocusCost cost targetID m =
  let target = findHero targetID m
   in adjustFocus cost target & updateHero m

adjustFocus :: Integer -> Hero -> Hero
adjustFocus value hero = hero {focusAmount = focusAmount hero + value}

findLeftHeroID :: Model -> HeroID
findLeftHeroID m =
  case find (\h -> battleSide h == LeftSide) (m & heroes) of
    Just hero -> heroID hero
    Nothing -> error "i tried to find a hero on the left battle side and failed"

findRightHeroID :: Model -> HeroID
findRightHeroID m =
  case find (\h -> battleSide h == RightSide) (m & heroes) of
    Just hero -> heroID hero
    Nothing ->
      error "i tried to find a hero on the right battle side and failed"

findHero :: HeroID -> Model -> Hero
findHero hID m =
  case find (\h -> heroID h == hID) (m & heroes) of
    Just hero -> hero
    Nothing ->
      error $ "i tried to find a hero by id " ++ show hID ++ " and failed"

handleAbilityAnimation :: Ability -> HeroID -> Model -> Model
handleAbilityAnimation Slash = setHeroAnimation Slashing
handleAbilityAnimation Hack = setHeroAnimation Hacking
