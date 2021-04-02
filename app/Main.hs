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
import Language.Javascript.JSaddle.Warp as JSaddle
import qualified Network.Wai as Wai
import Network.Wai.Application.Static
import qualified Network.Wai.Handler.Warp as Warp
import Network.WebSockets

-- end jsaddle import
import Control.Monad.IO.Class

import Data.Aeson
import qualified Data.Map as M
import Debug.Trace

data Hero =
  Hero
    { name :: String
    , health :: Integer
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

-- | initial model values at startup
initialModel :: Model
initialModel =
  Model
    { player =
        Hero {name = "bro", health = 1000, slashing = False, dead = False}
    , enemy =
        Hero
          {name = "the baddies", health = 1000, slashing = False, dead = False}
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
  checkIfBattleFinished $
  handleAnimation (Slash ht) $ handleDamage (Slash ht) $ m {playerTurn = False}
updateModel (SlashEnd ht) m =
  noEff $
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
    Player -> m {enemy = applyDamage (enemy m) 100}
    Enemy -> m {player = applyDamage (player m) 100}
handleDamage _ m = m

handleEnemyTurn :: Model -> Model
handleEnemyTurn m =
  if dead $ enemy m
    then m
    else handleAnimation (Slash Enemy) $ handleDamage (Slash Enemy) m

applyDamage :: Hero -> Integer -> Hero
applyDamage hero damage = checkIfDead $ hero {health = health hero - damage}

checkIfDead :: Hero -> Hero
checkIfDead hero =
  if health hero <= 0
    then hero {dead = True, health = 0}
    else hero

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
                    [class_ "health-text player"]
                    [text $ ms $ show $ health $ player m]
                , div_
                    [ class_ "health-bar player"
                    , style_ $ M.fromList [("width", healthBarWidth m Player)]
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
                    [class_ "health-text enemy"]
                    [text $ ms $ show $ health $ enemy m]
                , div_
                    [ class_ "health-bar enemy"
                    , style_ $ M.fromList [("width", healthBarWidth m Enemy)]
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
        [ classList_
            [ ("ability-button", True)
            , ("enabled", playerTurn m || battleFinished m)
            ]
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
    ]

healthBarWidth :: Model -> HeroType -> MisoString
healthBarWidth m herotype =
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
