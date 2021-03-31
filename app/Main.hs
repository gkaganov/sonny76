{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

module Main
  ( main
  ) where

-- miso framework import
import Miso
import Miso.String
-- jsaddle import depending on the compiler
#ifndef __GHCJS__
import Language.Javascript.JSaddle.Warp as JSaddle
import qualified Network.Wai as Wai
import Network.Wai.Application.Static
import qualified Network.Wai.Handler.Warp as Warp
import Network.WebSockets
#endif
-- IO Monad
import Control.Monad.IO.Class

import Data.Aeson
import qualified Data.Map as M
import Data.Text
import Data.Text.Conversions

-- import Data.Text (Text)
gridSize :: Integer
gridSize = 5

gridSpacing :: Integer
gridSpacing = 50

tileSize :: Integer
tileSize = 150

data Hero =
  Hero
    { name :: String
    , ability1 :: Ability
    }
  deriving (Eq)

data Tile =
  Tile
    { pos :: Pos
    , hero :: Maybe Hero
    , selected :: Bool
    , hovered :: Bool
    }
  deriving (Eq)

data Pos =
  Pos
    { x :: Int
    , y :: Int
    }
  deriving (Eq)

newtype Grid =
  Grid
    { tiles :: [Tile]
    }
  deriving (Eq)

data Ability
  = Shatter
  | Fireball
  deriving (Eq)

cast :: Ability -> Grid -> Tile -> Tile -> Grid
cast Fireball grid _ _ = grid
cast Shatter grid _ _ = grid

-- | Type synonym for an application model
data Model =
  Model
    { grid :: Grid
    , player :: Hero
    , enemy :: Hero
    , slashing :: Bool
    }
  deriving (Eq)

-- | Sum type for application events
data Action
  = NoOp
  | SlashStart
  | SlashEnd
  | PrintText Text
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
  let playerWithPos =
        (Hero {name = "bro", ability1 = Fireball}, Pos {x = 0, y = 0})
      enemyWithPos =
        (Hero {name = "the baddies", ability1 = Shatter}, Pos {x = 4, y = 4})
      pos = snd playerWithPos
   in Model
        { grid =
            Grid
              [ Tile
                  {pos = pos, hero = Nothing, selected = False, hovered = False}
              ]
        , player = fst playerWithPos
        , enemy = fst enemyWithPos
        , slashing = False
        }

-- | Entry point for a miso application
main :: IO ()
main = runApp $ startApp App {..}
  where
    initialAction = NoOp -- initial action to be executed on application load
    model = initialModel -- initial model
    update = updateModel -- update function
    view = viewModel -- view function
    events = M.insert "animationend" True defaultEvents -- default delegated events
    subs = [] -- empty subscription list
    mountPoint = Nothing -- mount point for application (Nothing defaults to 'body')
    logLevel = Off -- used during prerendering to see if the VDOM and DOM are in synch (only used with `miso` function)

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp m = noEff m
updateModel SlashStart m = noEff m {slashing = True}
updateModel SlashEnd m = noEff m {slashing = False}
updateModel (PrintText t) m =
  m <# do liftIO (putStrLn $ fromText t) >> pure NoOp

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
viewModel model =
  body_
    []
    [ link_ [rel_ "stylesheet", href_ "assets/css/style.css"]
    , link_ [rel_ "icon", href_ "assets/favicon.ico"]
    , div_
        [class_ "container"]
        [ h1_ [class_ "text"] [text "sonny76"]
        , div_
            [class_ "grid"]
            [ div_
                [class_ "hero-box"]
                [ div_
                    [ class_
                        (if slashing model
                           then "slashing hero player"
                           else "idling hero player")
                    , animationEndHandler ()
                    ]
                    []
                , p_ [class_ "text"] [text $ ms $ name $ player model]
                ]
            , div_
                [class_ "hero-box"]
                [ div_ [class_ "idling hero enemy"] []
                , p_ [class_ "text"] [text $ ms $ name $ enemy model]
                ]
            ]
        , div_ [class_ "ability-button"] [p_ [onClick SlashStart] [text "slash"]]
        ]
    ]
