module Abilities
  ( Ability(..)
  , canCast
  , cast
  , findHero
  , setHeroAnimation
  ) where

import Data.Foldable
import Data.Function
import Data.Sequence
import Model

focusCost :: Ability -> Integer
focusCost Slash = 0
focusCost Hack = 40

data Ability
  = Slash
  | Hack
  deriving (Eq)

instance Show Ability where
  show Slash = "slash"
  show Hack = "hack"

cast :: Ability -> HeroID -> HeroID -> Model -> Model
cast Slash _ target m =
  let damage = 150
   in m & applyDamage damage target
cast Hack caster target m =
  let damage = 350
      fCost = focusCost Hack
   in m & applyFocusCost fCost caster & applyDamage damage target

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

setHeroAnimation :: HeroID -> HeroAnimation -> Model -> Model
setHeroAnimation hID anim m =
  findHero hID m & (\h -> h {currentAnimation = anim}) & updateHero m

updateHero :: Model -> Hero -> Model
updateHero m newHero =
  case findIndexL (\h -> heroID h == heroID newHero) (m & heroes) of
    Just i -> m {heroes = update i newHero (m & heroes)}
    Nothing ->
      error $
      "i tried to update a hero by id " ++
      show (heroID newHero) ++ " and failed. shame on you!"

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

findHero :: HeroID -> Model -> Hero
findHero hID m =
  case find (\h -> heroID h == hID) (m & heroes) of
    Just hero -> hero
    Nothing ->
      error $
      "i tried to find a hero by id " ++
      show hID ++ " and failed. shame on you!"
