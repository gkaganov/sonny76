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
import Model

import Miso
import Miso.String (MisoString, ms)

import Control.Monad.IO.Class

import Data.Function

import Data.Aeson as Aeson -- json for event decoders
import qualified Data.Map as M

-- import Debug.Trace
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

newtype GuiEvent =
  AttackAnimationEndEvent HeroID

data Action
  = NoOp
  | Print String
  | AbilityBtnPressed Integer
  | AttackAnimationEnd HeroID
  | Restart
  deriving (Show, Eq)

initialModel :: Model
initialModel =
  Model
    { heroes =
        fromList
          [ Hero
              { name = "bro"
              , health = 1000
              , focusAmount = 100
              , currentAnimation = Idling
              , dead = False
              }
          , Hero
              { name = "the baddies"
              , health = 1000
              , focusAmount = 100
              , currentAnimation = Idling
              , dead = False
              }
          ]
    , battleFinished = False
    , humanActive = True
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
updateModel (AttackAnimationEnd hID) m =
  m & handleGuiEventAnimation (AttackAnimationEndEvent hID) &
  (case findHero hID m & battleSide of
     LeftSide -> enemyTurn
     RightSide -> \m' -> m' {humanActive = True}) &
  determineIfBattleFinished &
  noEff
updateModel Restart _ = initialModel & noEff

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

handleAbilityAnimation :: Ability -> HeroID -> Model -> Model
handleAbilityAnimation Slash hID m = setHeroAnimation hID Slashing
handleAbilityAnimation Hack hID m = setHeroAnimation hID Hacking

handleGuiEventAnimation :: GuiEvent -> Model -> Model
handleGuiEventAnimation (AttackAnimationEndEvent hID) =
  setHeroAnimation hID Idling

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
                , ("enabled", humanActive m || battleFinished m)
                ]
            ]
            [ p_
                [ onClick $
                  if battleFinished m
                    then Restart
                    else if humanActive m
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
                  , (humanActive m && canCast Hack m Player) || battleFinished m)
                ]
            ]
            [ p_
                [ onClick $
                  if battleFinished m
                    then Restart
                    else if humanActive m && focusAmount (player m) >= 40
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

buildHealthBarWidth :: Model -> HeroID -> MisoString
buildHealthBarWidth m hID =
  ms
    ("calc(" ++
     "var(--health-bar-width) * " ++
     "(" ++ show (health $ findHero hID m Model) ++ "/" ++ show 1000 ++ "))")

calculateHealthBarColorLabel :: Hero -> (MisoString, Bool)
calculateHealthBarColorLabel hero =
  if | health hero > 500 -> ("full", True)
     | health hero > 250 -> ("medium", True)
     | otherwise -> ("low", True)

buildFocusBarWidth :: Model -> HeroID -> MisoString
buildFocusBarWidth m hID =
  ms
    ("calc(" ++ "var(--focus-bar-width) * " ++ "(" ++ show focusAmount $
     findHero hID m ++ "/" ++ show 100 ++ "))")
