module Update
  ( updateModel
  ) where

import Ability
import AbilityImpl
import BuffImpl
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

humanTurn :: Int -> Model -> Model
humanTurn abilityNum m =
  let heroes = m & heroesInBattle
      hID = findHeroID LeftSide heroes
      hero = findHero hID heroes
      ability = (hero & availableAbilities) !! abilityNum
      enemy = findHeroID RightSide heroes
   in if dead hero
        then m
        else m
               { humanActive = False
               , heroesInBattle =
                   heroes & triggerAllBuffs hID & cast ability hID enemy &
                   handleAbilityAnimation ability hID
               }

aiTurn :: Model -> Model
aiTurn m =
  let heroes = m & heroesInBattle
      hID = findHeroID RightSide heroes
      hero = findHero hID heroes
      enemy = findHeroID LeftSide heroes
   in if dead hero
        then m
        else let heroes' = triggerAllBuffs hID heroes
                 ability =
                   aiChooseAbility hID (hero & availableAbilities) heroes'
              in m
                   { heroesInBattle =
                       heroes' & cast ability hID enemy &
                       handleAbilityAnimation ability hID
                   }

aiChooseAbility :: HeroID -> [Ability] -> [Hero] -> Ability
aiChooseAbility hID abilities heroes =
  let highPrio = last abilities
      lowPrio = head abilities
   in if canCast highPrio hID heroes
        then highPrio
        else lowPrio

determineIfBattleFinished :: Model -> Model
determineIfBattleFinished m =
  let heroes = m & heroesInBattle
   in if dead (findHero (findHeroID LeftSide heroes) heroes)
        then m {battleWinner = Just RightSide}
        else if dead (findHero (findHeroID RightSide heroes) heroes)
               then m {battleWinner = Just LeftSide}
               else m

handleGuiEventAnimation :: GuiEvent -> Model -> Model
handleGuiEventAnimation (AttackAnimationEndEvent hID) m =
  m {heroesInBattle = setHeroAnimation Idling hID (m & heroesInBattle)}
