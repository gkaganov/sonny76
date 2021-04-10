{-# LANGUAGE MultiWayIf #-}
module View
  ( viewModel
  ) where

import Model
import Hero

import Miso
import qualified Data.Map as M
import Miso.String (MisoString, ms)

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
                  , (humanActive m && canCast Hack m (findRightHeroID m)) ||
                    battleFinished m)
                ]
            ]
            [ p_
                [ onClick $
                  if battleFinished m
                    then Restart
                    else if humanActive m &&
                            focusAmount (findHero (findRightHeroID m) m) >= 40
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
     "(" ++ show (health $ findHero hID m) ++ "/" ++ show 1000 ++ "))")

calculateHealthBarColorLabel :: Hero -> (MisoString, Bool)
calculateHealthBarColorLabel hero =
  if | health hero > 500 -> ("full", True)
     | health hero > 250 -> ("medium", True)
     | otherwise -> ("low", True)

buildFocusBarWidth :: Model -> HeroID -> MisoString
buildFocusBarWidth m hID =
  ms
    ("calc(" ++
     "var(--focus-bar-width) * " ++
     "(" ++ show (focusAmount $ findHero hID m) ++ "/" ++ show 100 ++ "))")
