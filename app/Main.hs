{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main
  ( main
  ) where

import Hero
import Model
import View

import Control.Monad.IO.Class
import Data.Aeson as Aeson -- json for event decoders
import Data.Function
import qualified Data.Map as M
import Miso

-- jsaddle for local dev
import Language.Javascript.JSaddle.Warp as JSaddle
import qualified Network.Wai as Wai
import Network.Wai.Application.Static
import qualified Network.Wai.Handler.Warp as Warp
import Network.WebSockets

runApp :: JSM () -> IO ()
runApp f =
  Warp.runSettings
    (Warp.setPort 8080 (Warp.setTimeout 3600 Warp.defaultSettings)) =<<
  JSaddle.jsaddleOr defaultConnectionOptions (f >> syncPoint) app
  where
    app req sendResp =
      case Wai.pathInfo req of
        ("assets":_) -> staticApp (defaultWebAppSettings ".") req sendResp
        _ -> JSaddle.jsaddleApp req sendResp

main :: IO ()
main = runApp $ startApp App {..}
  where
    initialAction = NoOp
    model = initialModel
    update = updateModel
    view = viewModel
    events =
      defaultEvents & M.insert "transitionend" True &
      M.insert "animationend" True
    subs = []
    mountPoint = Nothing -- mount on body
    logLevel = Off -- miso internal

-- Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp m = noEff m
updateModel (Print t) m = m <# do liftIO (putStrLn t) >> pure NoOp
updateModel (AbilityBtnPressed n) m = m & playerTurn n & noEff
updateModel (AttackAnimationEnd hID) m =
  m & handleGuiEventAnimation (AttackAnimationEndEvent hID) &
  (case findHero hID m & battleSide of
     LeftSide -> enemyTurn
     RightSide -> \m' -> m' {humanActive = True}) &
  determineIfBattleFinished &
  noEff
updateModel Restart _ = initialModel & noEff

handleGuiEventAnimation :: GuiEvent -> Model -> Model
handleGuiEventAnimation (AttackAnimationEndEvent hID) =
  setHeroAnimation hID Idling

animationNameDecoder :: Decoder Aeson.Value
animationNameDecoder =
  Decoder
    { decodeAt = DecodeTarget mempty
    , decoder = Aeson.withObject "event" $ \o -> o .: "animationName"
    }

animationEndHandler :: HeroID -> Attribute Action
animationEndHandler heroID =
  Miso.on "animationend" animationNameDecoder $ \case
    Aeson.String name ->
      case name of
        "left-slash" -> AttackAnimationEnd herotype
        "right-slash" -> AttackAnimationEnd herotype
        "left-hack" -> AttackAnimationEnd herotype
        "right-hack" -> AttackAnimationEnd herotype
        _ -> NoOp
    _ -> error "Unexpected case"
