{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Battle where

import AbilityData
import AbilityList
import Hero
import Model

import Data.Function ((&))

humanTurn :: Integer -> Model -> Model
humanTurn abilityNum m =
  let ability =
        case abilityNum of
          0 -> Slash
          1 -> Hack
          n -> error $ "there is no ability defined for slot " ++ show n
      hero = findHero (findHeroID LeftSide m) m
      enemy = findHero (findHeroID RightSide m) m
   in if dead hero
        then m
        else m {humanActive = False} & cast ability (heroID hero) (heroID enemy) &
             handleAbilityAnimation ability (heroID hero)

aiTurn :: Model -> Model
aiTurn m =
  let ability = Slash
      hero = findHero (findHeroID RightSide m) m
      enemy = findHero (findHeroID LeftSide m) m
   in if dead hero
        then m
        else cast ability (heroID hero) (heroID enemy) m &
             handleAbilityAnimation ability (heroID hero)

determineIfBattleFinished :: Model -> Model
determineIfBattleFinished m =
  if dead (findHero (findHeroID LeftSide m) m) ||
     dead (findHero (findHeroID RightSide m) m)
    then m {battleFinished = True}
    else m
