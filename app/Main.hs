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
    , slashing :: Bool
    }
  deriving (Show, Eq)

-- | Type synonym for an application model
data Model =
  Model
    { player :: Hero
    , enemy :: Hero
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
    { player = Hero {name = "bro", health = 1000, slashing = False}
    , enemy = Hero {name = "the baddies", health = 1000, slashing = False}
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
  noEff $ handleAnimation (Slash ht) $ handleDamage (Slash ht) m
updateModel (SlashEnd ht) m = noEff $ handleAnimation (SlashEnd ht) m

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
handleEnemyTurn m = handleAnimation (Slash Enemy) $ handleDamage (Slash Enemy) m

applyDamage :: Hero -> Integer -> Hero
applyDamage hero damage = hero {health = health hero - damage}

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
    , h1_ [class_ "text"] [text "sonny76"]
    , div_
        [class_ "horizontal battle-sides"]
        [ div_
            [class_ "vertical hero-box"]
            [ div_
                [ class_ "health-bar-container"
                , style_ $ M.fromList [("width", "var(--health-bar-width)")]
                ]
                [ div_
                    [class_ "health-text"]
                    [text $ ms $ show $ health $ player m]
                , div_ [class_ "health-bar"] []
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
                    [class_ "health-text"]
                    [text $ ms $ show $ health $ enemy m]
                , div_
                    [ class_ "health-bar"
                    , style_ $ M.fromList [("width", healthBarWidth m)]
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
        [class_ "ability-button"]
        [p_ [onClick $ Slash Player] [text "slash"]]
    ]

healthBarWidth :: Model -> MisoString
healthBarWidth m =
  ms
    ("calc(" ++
     "var(--health-bar-width) * " ++
     "(" ++ show (health $ enemy m) ++ "/" ++ show 1000 ++ ")")
