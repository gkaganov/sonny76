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
    }
  deriving (Eq)

-- | Sum type for application events
data Action
  = AddOne
  | SubtractOne
  | NoOp
  | SayHelloWorld
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
        (Hero {name = "hans", ability1 = Fireball}, Pos {x = 0, y = 0})
      enemyWithPos =
        (Hero {name = "knight", ability1 = Shatter}, Pos {x = 4, y = 4})
      pos = snd playerWithPos
   in Model
        { grid =
            Grid
              [ Tile
                  {pos = pos, hero = Nothing, selected = False, hovered = False}
              ] -- List.initialize (gridSize * gridSize) (makeTile playerWithPos enemyWithPos)
        , player = fst playerWithPos
        , enemy = fst enemyWithPos
        }

-- | Entry point for a miso application
main :: IO ()
main = runApp $ startApp App {..}
  where
    initialAction = SayHelloWorld -- initial action to be executed on application load
    model = initialModel -- initial model
    update = updateModel -- update function
    view = viewModel -- view function
    events = defaultEvents -- default delegated events
    subs = [] -- empty subscription list
    mountPoint = Nothing -- mount point for application (Nothing defaults to 'body')
    logLevel = Off -- used during prerendering to see if the VDOM and DOM are in synch (only used with `miso` function)

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel AddOne m = noEff m
updateModel SubtractOne m = noEff m
updateModel NoOp m = noEff m
updateModel SayHelloWorld m =
  m <# do liftIO (putStrLn "Hello World") >> pure NoOp

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel x =
  body_
    []
    [ link_ [rel_ "stylesheet", href_ "assets/style.css"]
    , link_ [rel_ "icon", href_ "assets/favicon.ico"]
    , div_
        [class_ "grid"]
        [ div_
            [class_ "hero-box"]
            [ div_ [class_ "player"] []
            , div_ [class_ "hero-name"] [text (ms $ name $ player x)]
            ]
        , div_
            [class_ "hero-box"]
            [ div_ [class_ "enemy"] []
            , div_ [class_ "hero-name"] [text (ms $ name $ enemy x)]
            ]
        ]
    ]
