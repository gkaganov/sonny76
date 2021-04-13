module Update
  ( updateModel
  ) where

import Ability
import AbilityImpl
import Hero
import Model

import Miso.Effect

import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))

-- Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp m = noEff m
updateModel (Print t) m = m <# do liftIO (putStrLn t) >> pure NoOp
updateModel (AbilityBtnPressed n) m = m & humanTurn n & noEff
updateModel (AttackAnimationEnd hID) m =
  m & handleGuiEventAnimation (AttackAnimationEndEvent hID) &
  (case m & heroesInBattle & findHero hID & battleSide of
     LeftSide -> aiTurn
     RightSide -> \m' -> m' {humanActive = True}) &
  determineIfBattleFinished &
  noEff
updateModel Restart _ = initialModel & noEff

humanTurn :: Integer -> Model -> Model
humanTurn abilityNum m =
  let ability =
        case abilityNum of
          0 -> Slash
          1 -> Hack
          n -> error $ "there is no ability defined for slot " ++ show n
      heroes = m & heroesInBattle
      hero = findHero (findHeroID LeftSide heroes) heroes
      enemy = findHero (findHeroID RightSide heroes) heroes
   in if dead hero
        then m
        else m
               { humanActive = False
               , heroesInBattle =
                   heroes & cast ability (heroID hero) (heroID enemy) &
                   handleAbilityAnimation ability (heroID hero)
               }

aiTurn :: Model -> Model
aiTurn m =
  let ability = Slash
      heroes = m & heroesInBattle
      hero = findHero (findHeroID RightSide heroes) heroes
      enemy = findHero (findHeroID LeftSide heroes) heroes
   in if dead hero
        then m
        else m
               { heroesInBattle =
                   m & heroesInBattle &
                   cast ability (heroID hero) (heroID enemy) &
                   handleAbilityAnimation ability (heroID hero)
               }

determineIfBattleFinished :: Model -> Model
determineIfBattleFinished m =
  let heroes = m & heroesInBattle
   in if dead (findHero (findHeroID LeftSide heroes) heroes) ||
         dead (findHero (findHeroID RightSide heroes) heroes)
        then m {battleFinished = True}
        else m

handleGuiEventAnimation :: GuiEvent -> Model -> Model
handleGuiEventAnimation (AttackAnimationEndEvent hID) m =
  m {heroesInBattle = setHeroAnimation Idling hID (m & heroesInBattle)}
