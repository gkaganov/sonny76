{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Battle where

import Model
import Ability

playerTurn :: Integer -> Model -> Model
playerTurn abilityNum m =
  let ability =
        case abilityNum of
          0 -> Slash
          1 -> Hack
          n -> error $ "there is no ability defined for slot " ++ show n
   in if m & player & dead
        then m
        else m {humanActive = False} & cast ability Player Enemy &
             handleAbilityAnimation ability Player

enemyTurn :: Model -> Model
enemyTurn m =
  let ability = Slash
   in if m & enemy & dead
        then m
        else cast ability Enemy Player m & handleAbilityAnimation ability Enemy

determineIfBattleFinished :: Model -> Model
determineIfBattleFinished m =
  if dead (player m) || dead (enemy m)
    then m {battleFinished = True}
    else m
