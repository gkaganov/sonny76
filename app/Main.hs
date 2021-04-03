{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main
  ( main
  ) where

import Abilities
import Types

import Miso
import Miso.String

import Control.Monad.IO.Class

import Data.Function

import Data.Aeson as Aeson -- json for event decoders
import qualified Data.Map as M
-- import Debug.Trace
-- jsaddle for local dev
#ifndef __GHCJS__
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
#else
-- ghcjs for deployment
runApp :: IO () -> IO ()
runApp app = app
#endif


newtype GuiEvent =
  AttackAnimationEndEvent HeroType


data Action
  = NoOp
  | Print String
  | AbilityBtnPressed Integer
  | AttackAnimationEnd HeroType
  | Restart
  deriving (Show, Eq)

initialModel :: Model
initialModel =
  Model
    { player =
        Hero
          { name = "bro"
          , health = 1000
          , focusAmount = 100
          , slashing = False
          , hacking = False
          , dead = False
          }
    , enemy =
        Hero
          { name = "the baddies"
          , health = 1000
          , focusAmount = 100
          , slashing = False
          , hacking = False
          , dead = False
          }
    , battleFinished = False
    , playerActive = True
    }

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
updateModel (AttackAnimationEnd ht) m =
  m & handleGuiEventAnimation (AttackAnimationEndEvent ht) &
  (case ht of
     Player -> enemyTurn
     Enemy -> \m' -> m' {playerActive = True}) &
  determineIfBattleFinished &
  noEff
updateModel Restart _ = initialModel & noEff

playerTurn :: Integer -> Model -> Model
playerTurn abilityNum m =
  let ability =
        case abilityNum of
          0 -> slash
          1 -> hack
          _ -> Ability {abilityName = "", action = \_ _ m' -> m'}
   in if m & player & dead
        then m
        else action ability Player Enemy m &
             handleAbilityAnimation (abilityName ability) Player & \m' ->
               m' {playerActive = False}

enemyTurn :: Model -> Model
enemyTurn m =
  let ability = slash
   in if m & enemy & dead
        then m
        else action ability Enemy Player m &
             handleAbilityAnimation (abilityName ability) Enemy

handleAbilityAnimation :: String -> HeroType -> Model -> Model
handleAbilityAnimation name ht m =
  case ht of
    Player ->
      case name of
        "slash" -> m {player = (player m) {slashing = True}}
        "hack" -> m {player = (player m) {hacking = True}}
        _ -> m
    Enemy ->
      case name of
        "slash" -> m {enemy = (enemy m) {slashing = True}}
        "hack" -> m {enemy = (enemy m) {hacking = True}}
        _ -> m

handleGuiEventAnimation :: GuiEvent -> Model -> Model
handleGuiEventAnimation (AttackAnimationEndEvent Player) m =
  m {player = (player m) {slashing = False, hacking = False}}
handleGuiEventAnimation (AttackAnimationEndEvent Enemy) m =
  m {enemy = (enemy m) {slashing = False, hacking = False}}

determineIfBattleFinished :: Model -> Model
determineIfBattleFinished m =
  if dead (player m) || dead (enemy m)
    then m {battleFinished = True}
    else m

animationNameDecoder :: Decoder Aeson.Value
animationNameDecoder =
  Decoder
    { decodeAt = DecodeTarget mempty
    , decoder = Aeson.withObject "event" $ \o -> o .: "animationName"
    }

animationEndHandler :: HeroType -> Attribute Action
animationEndHandler herotype =
  Miso.on "animationend" animationNameDecoder $ \case
    Aeson.String name ->
      case name of
        "left-slash" -> AttackAnimationEnd herotype
        "right-slash" -> AttackAnimationEnd herotype
        "left-hack" -> AttackAnimationEnd herotype
        "right-hack" -> AttackAnimationEnd herotype
        _ -> NoOp
    _ -> error "Unexpected case"

-- Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel m =
  div_
    [class_ "vertical root"]
    [ link_ [rel_ "stylesheet", href_ "assets/css/style.css"]
    , link_ [rel_ "icon", href_ "assets/favicon.ico"]
    , h3_
        [class_ "text"]
        [ text $
          if battleFinished m
            then "YOU WIN"
            else "sonny76"
        ]
    , div_
        [class_ "horizontal battle-sides"]
        [ div_
            [class_ "vertical hero-box"]
            [ div_
                [ class_ "health-bar-container"
                , style_ $
                  M.fromList [("width", "var(--health-bar-container-width)")]
                ]
                [ div_
                    [class_ "health-amount player"]
                    [text $ ms $ show $ health $ player m]
                , div_
                    [ classList_
                        [ ("health-bar player", True)
                        , calculateHealthBarColorLabel $ player m
                        ]
                    , style_ $
                      M.fromList [("width", buildHealthBarWidth m Player)]
                    ]
                    []
                ]
            , div_
                [ class_ "focus-bar-container"
                , style_ $
                  M.fromList [("width", "var(--focus-bar-container-width)")]
                ]
                [ div_
                    [class_ "focus-amount player"]
                    [text $ ms $ show $ focusAmount $ player m]
                , div_
                    [ class_ "focus-bar player"
                    , style_ $
                      M.fromList [("width", buildFocusBarWidth m Player)]
                    ]
                    []
                ]
            , div_
                [ classList_
                    [ ("hero player", True)
                    , if | slashing $ player m -> ("slashing", True)
                         | hacking $ player m -> ("hacking", True)
                         | otherwise -> ("idling", True)
                    ]
                , animationEndHandler Player
                ]
                []
            , p_ [class_ "text"] [text $ ms $ name $ player m]
            ]
        , div_
            [class_ "vertical hero-box"]
            [ div_
                [ class_ "health-bar-container"
                , style_ $
                  M.fromList [("width", "var(--health-bar-container-width)")]
                ]
                [ div_
                    [class_ "health-amount enemy"]
                    [text $ ms $ show $ health $ enemy m]
                , div_
                    [ classList_
                        [ ("health-bar enemy", True)
                        , calculateHealthBarColorLabel $ enemy m
                        ]
                    , style_ $
                      M.fromList [("width", buildHealthBarWidth m Enemy)]
                    ]
                    []
                ]
            , div_
                [ class_ "focus-bar-container"
                , style_ $
                  M.fromList [("width", "var(--focus-bar-container-width)")]
                ]
                [ div_
                    [class_ "focus-amount enemy"]
                    [text $ ms $ show $ focusAmount $ enemy m]
                , div_
                    [ class_ "focus-bar enemy"
                    , style_ $
                      M.fromList [("width", buildFocusBarWidth m Enemy)]
                    ]
                    []
                ]
            , div_
                [ class_
                    (if slashing $ enemy m
                       then "slashing hero enemy"
                       else "idling hero enemy")
                , animationEndHandler Enemy
                ]
                []
            , p_ [class_ "text"] [text $ ms $ name $ enemy m]
            ]
        ]
    , div_
        [class_ "horizontal ability-bar"]
        [ div_
            [ classList_
                [ ("ability-button", True)
                , ("enabled", playerActive m || battleFinished m)
                ]
            ]
            [ p_
                [ onClick $
                  if battleFinished m
                    then Restart
                    else if playerActive m
                           then AbilityBtnPressed 0
                           else NoOp
                ]
                [ text $
                  if battleFinished m
                    then "restart"
                    else "slash"
                ]
            ]
        , div_
            [ classList_
                [ ("ability-button", True)
                , ( "enabled"
                  , (playerActive m && focusAmount (player m) >= 40) ||
                    battleFinished m)
                ]
            ]
            [ p_
                [ onClick $
                  if battleFinished m
                    then Restart
                    else if playerActive m && focusAmount (player m) >= 40
                           then AbilityBtnPressed 1
                           else NoOp
                ]
                [ text $
                  if battleFinished m
                    then "restart"
                    else "hack"
                ]
            ]
        ]
    ]

buildHealthBarWidth :: Model -> HeroType -> MisoString
buildHealthBarWidth m herotype =
  ms
    ("calc(" ++
     "var(--health-bar-width) * " ++
     "(" ++
     show
       (health $
        case herotype of
          Player -> player m
          Enemy -> enemy m) ++
     "/" ++ show 1000 ++ "))")

calculateHealthBarColorLabel :: Hero -> (MisoString, Bool)
calculateHealthBarColorLabel hero =
  if | health hero > 500 -> ("full", True)
     | health hero > 250 -> ("medium", True)
     | otherwise -> ("low", True)

buildFocusBarWidth :: Model -> HeroType -> MisoString
buildFocusBarWidth m herotype =
  ms
    ("calc(" ++
     "var(--focus-bar-width) * " ++
     "(" ++
     show
       (focusAmount $
        case herotype of
          Player -> player m
          Enemy -> enemy m) ++
     "/" ++ show 100 ++ "))")
