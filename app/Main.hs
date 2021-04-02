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

import Miso
import Miso.String
-- jsaddle import for local dev
#ifndef __GHCJS__
import Language.Javascript.JSaddle.Warp as JSaddle
import qualified Network.Wai as Wai
import Network.Wai.Application.Static
import qualified Network.Wai.Handler.Warp as Warp
import Network.WebSockets
#endif
-- end jsaddle import
import Control.Monad.IO.Class

import Data.Aeson
import qualified Data.Map as M
import Debug.Trace

data Hero =
  Hero
    { name :: String
    , health :: Integer
    , focusAmount :: Integer
    , slashing :: Bool
    , dead :: Bool
    }
  deriving (Show, Eq)

-- | Type synonym for an application model
data Model =
  Model
    { player :: Hero
    , enemy :: Hero
    , battleFinished :: Bool
    , playerTurn :: Bool
    }
  deriving (Show, Eq)

data HeroType
  = Player
  | Enemy
  deriving (Show, Eq)

-- | Sum type for application events
data Action
  = NoOp
  | Print String
  | Slash HeroType
  | SlashEnd HeroType
  | Restart
  deriving (Show, Eq)
-- | jsaddle runApp
#ifndef __GHCJS__
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
-- | ghcjs runApp
runApp :: IO () -> IO ()
runApp app = app
#endif
-- | initial model values at startup
initialModel :: Model
initialModel =
  Model
    { player =
        Hero
          { name = "bro"
          , health = 1000
          , focusAmount = 100
          , slashing = False
          , dead = False
          }
    , enemy =
        Hero
          { name = "the baddies"
          , health = 1000
          , focusAmount = 100
          , slashing = False
          , dead = False
          }
    , battleFinished = False
    , playerTurn = True
    }

-- | Entry point for a miso application
main :: IO ()
main = runApp $ startApp App {..}
  where
    initialAction = NoOp -- initial action to be executed on application load
    model = initialModel -- initial model
    update = updateModel -- update function
    view = viewModel -- view function
    events =
      M.insert "transitionend" True $
      M.insert "animationend" True defaultEvents -- default delegated events
    subs = [] -- empty subscription list
    mountPoint = Nothing -- mount point for application (Nothing defaults to 'body')
    logLevel = Off -- used during prerendering to see if the VDOM and DOM are in synch (only used with `miso` function)

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp m = noEff m
updateModel (Print t) m = m <# do liftIO (putStrLn t) >> pure NoOp
updateModel (Slash ht) m =
  noEff $
  handleAnimation (Slash ht) $
  handleFocusCost (Slash ht) $ handleDamage (Slash ht) $ m {playerTurn = False}
updateModel (SlashEnd ht) m =
  noEff $
  checkIfBattleFinished $
  handleAnimation
    (SlashEnd ht)
    (case ht of
       Enemy -> m {playerTurn = True}
       Player -> m)
updateModel Restart _ = noEff initialModel

handleAnimation :: Action -> Model -> Model
handleAnimation (Slash herotype) m =
  traceShowId $
  case herotype of
    Player -> m {player = (player m) {slashing = True}}
    Enemy -> m {enemy = (enemy m) {slashing = True}}
handleAnimation (SlashEnd herotype) m =
  traceShowId $
  case herotype of
    Player -> handleEnemyTurn $ m {player = (player m) {slashing = False}}
    Enemy -> m {enemy = (enemy m) {slashing = False}}
handleAnimation _ m = m

handleDamage :: Action -> Model -> Model
handleDamage (Slash herotype) m =
  case herotype of
    Player -> m {enemy = applyDamage (enemy m) 200}
    Enemy -> m {player = applyDamage (player m) 150}
handleDamage _ m = m

handleFocusCost :: Action -> Model -> Model
handleFocusCost (Slash herotype) m =
  case herotype of
    Player -> m {player = subtractFocus (player m) 20}
    Enemy -> m {enemy = subtractFocus (enemy m) 10}
handleFocusCost _ m = m

handleEnemyTurn :: Model -> Model
handleEnemyTurn m =
  if dead $ enemy m
    then m
    else handleFocusCost (Slash Enemy) $
         handleAnimation (Slash Enemy) $ handleDamage (Slash Enemy) m

applyDamage :: Hero -> Integer -> Hero
applyDamage hero damage = checkIfDead $ hero {health = health hero - damage}

checkIfDead :: Hero -> Hero
checkIfDead hero =
  if health hero <= 0
    then hero {dead = True, health = 0}
    else hero

subtractFocus :: Hero -> Integer -> Hero
subtractFocus hero cost = hero {focusAmount = focusAmount hero - cost}

checkIfBattleFinished :: Model -> Model
checkIfBattleFinished m =
  if dead (player m) || dead (enemy m)
    then m {battleFinished = True}
    else m

animationNameDecoder :: Decoder Value
animationNameDecoder =
  Decoder
    { decodeAt = DecodeTarget mempty
    , decoder = withObject "event" $ \o -> o .: "animationName"
    }

animationEndHandler :: HeroType -> Attribute Action
animationEndHandler herotype =
  on "animationend" animationNameDecoder $ \case
    String name ->
      case name of
        "left-slash" -> SlashEnd herotype
        "right-slash" -> SlashEnd herotype
        _ -> NoOp
    _ -> error "Unexpected case"

-- | Constructs a virtual DOM from a model
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
                    [ class_ "health-bar player"
                    , classList_ [calculateHealthBarColorLabel $ player m]
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
                [ class_
                    (if slashing $ player m
                       then "slashing hero player"
                       else "idling hero player")
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
                    [ class_ "health-bar enemy"
                    , classList_ [calculateHealthBarColorLabel $ enemy m]
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
            [ class_ "ability-button"
            , classList_ [("enabled", playerTurn m || battleFinished m)]
            ]
            [ p_
                [ onClick $
                  if battleFinished m
                    then Restart
                    else if playerTurn m
                           then Slash Player
                           else NoOp
                ]
                [ text $
                  if battleFinished m
                    then "restart"
                    else "slash"
                ]
            ]
        , div_
            [ class_ "ability-button"
            , classList_ [("enabled", playerTurn m || battleFinished m)]
            ]
            [ p_
                [ onClick $
                  if battleFinished m
                    then Restart
                    else if playerTurn m
                           then Slash Player
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
