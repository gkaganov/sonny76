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

-- jsaddle import depending on the compiler
import Language.Javascript.JSaddle.Warp as JSaddle
import qualified Network.Wai as Wai
import Network.Wai.Application.Static
import qualified Network.Wai.Handler.Warp as Warp
import Network.WebSockets

-- end jsaddle import
import Control.Monad.IO.Class

import Data.Aeson
import qualified Data.Map as M

data Hero =
  Hero
    { name :: String
    , health :: Integer
    , ability1 :: Ability
    }
  deriving (Eq)

data Ability
  = Shatter
  | Fireball
  deriving (Eq)

-- | Type synonym for an application model
data Model =
  Model
    { player :: Hero
    , enemy :: Hero
    , slashing :: Bool
    , healthChanging :: Bool
    , lastDamageApplied :: Integer
    }
  deriving (Eq)

-- | Sum type for application events
data Action
  = NoOp
  | Print String
  | Slash
  | SlashEnd
  | HealthFinishedChanging
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
    { player = Hero {name = "bro", health = 1000, ability1 = Fireball}
    , enemy = Hero {name = "the baddies", health = 1000, ability1 = Shatter}
    , slashing = False
    , healthChanging = False
    , lastDamageApplied = 0
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
updateModel a m = noEff $ handleAnimation a $ handleDamage a m

handleAnimation :: Action -> Model -> Model
handleAnimation Slash m = m {slashing = True, healthChanging = True}
handleAnimation SlashEnd m = m {slashing = False}
handleAnimation HealthFinishedChanging m = m {healthChanging = False}
handleAnimation _ m = m

handleDamage :: Action -> Model -> Model
handleDamage Slash m =
  m {enemy = applyDamage (enemy m) 100, lastDamageApplied = 100}
handleDamage _ m = m

applyDamage :: Hero -> Integer -> Hero
applyDamage hero damage = hero {health = health hero - damage}

animationNameDecoder :: Decoder Value
animationNameDecoder =
  Decoder
    { decodeAt = DecodeTarget mempty
    , decoder = withObject "event" $ \o -> o .: "animationName"
    }

animationEndHandler :: () -> Attribute Action
animationEndHandler () =
  on "animationend" animationNameDecoder $ \case
    String name ->
      case name of
        "slash" -> SlashEnd
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
                [div_ [class_ "health-bar"] [div_ [class_ "hit-bar"] []]]
            , div_
                [ class_
                    (if slashing m
                       then "slashing hero player"
                       else "idling hero player")
                , animationEndHandler ()
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
                [ div_ [class_ "health-text"] [text $ ms $ show $ health $ enemy m]
                , div_
                    [ class_ "health-bar"
                    , style_ $ M.fromList [("width", healthBarWidth m)]
                    ]
                    [ div_
                        [ class_ "hit-bar"
                        , on "transitionend" emptyDecoder $ \() ->
                            HealthFinishedChanging
                        , style_ $ M.fromList [("width", hitBarWidth m)]
                        ]
                        []
                    ]
                ]
            , div_ [class_ "idling hero enemy"] []
            , p_ [class_ "text"] [text $ ms $ name $ enemy m]
            ]
        ]
    , div_ [class_ "ability-button"] [p_ [onClick Slash] [text "slash"]]
    ]

healthBarWidth :: Model -> MisoString
healthBarWidth m =
  ms
    ("calc(" ++
     "var(--health-bar-width) * " ++
     "(" ++ show (health $ enemy m) ++ "/" ++ show 1000 ++ ")")

hitBarWidth :: Model -> MisoString
hitBarWidth m =
  ms
    ("calc(" ++
     "var(--health-bar-width) * " ++
     "(" ++
     (if healthChanging m
        then show (lastDamageApplied m)
        else "0") ++
     "/" ++ show 1000 ++ ")")
