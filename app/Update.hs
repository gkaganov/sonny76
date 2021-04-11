module Update
  ( updateModel
  ) where

import Battle
import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Hero
import Miso.Effect
-- Updates model, optionally introduces side effects
import Model

updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp m = noEff m
updateModel (Print t) m = m <# do liftIO (putStrLn t) >> pure NoOp
updateModel (AbilityBtnPressed n) m = m & humanTurn n & noEff
updateModel (AttackAnimationEnd hID) m =
  m & handleGuiEventAnimation (AttackAnimationEndEvent hID) &
  (case findHero hID m & battleSide of
     LeftSide -> aiTurn
     RightSide -> \m' -> m' {humanActive = True}) &
  determineIfBattleFinished &
  noEff
updateModel Restart _ = initialModel & noEff

handleGuiEventAnimation :: GuiEvent -> Model -> Model
handleGuiEventAnimation (AttackAnimationEndEvent hID) =
  setHeroAnimation Idling hID
