{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module View
  ( viewModel
  ) where

import Hero
import Model
import AbilityData

import Miso
import Miso.String (MisoString, ms)

import Data.Aeson (Value, (.:), withObject)

import qualified Data.Map as M

animationEndHandler :: HeroID -> Attribute Action
animationEndHandler hID =
  Miso.on "animationend" animationNameDecoder $ \case
    "left-slash" -> AttackAnimationEnd hID
    "right-slash" -> AttackAnimationEnd hID
    "left-hack" -> AttackAnimationEnd hID
    "right-hack" -> AttackAnimationEnd hID
    _ -> NoOp

animationNameDecoder :: Decoder Value
animationNameDecoder =
  Decoder
    { decodeAt = DecodeTarget mempty
    , decoder = withObject "event" $ \o -> o .: "animationName"
    }

buildBattleSide :: BattleSide -> Model -> View Action
buildBattleSide side m =
  let hero = findHero (findHeroID side m) m
   in div_
        [class_ "vertical hero-box"]
        [ div_
            [ class_ "health-bar-container"
            , style_ $
              M.fromList [("width", "var(--health-bar-container-width)")]
            ]
            [ div_
                [class_ $ ms $ "health-amount " ++ show side]
                [text $ ms $ show $ health hero]
            , div_
                [ classList_
                    [ (ms $ "health-bar " ++ show side, True)
                    , calculateHealthBarColorLabel hero
                    ]
                , style_ $ M.fromList [("width", buildHealthBarWidth hero)]
                ]
                []
            ]
        , div_
            [ class_ "focus-bar-container"
            , style_ $
              M.fromList [("width", "var(--focus-bar-container-width)")]
            ]
            [ div_
                [class_ $ ms $ "focus-amount " ++ show side]
                [text $ ms $ show $ focusAmount hero]
            , div_
                [ class_ $ ms $ "focus-bar " ++ show side
                , style_ $ M.fromList [("width", buildFocusBarWidth hero)]
                ]
                []
            ]
        , div_
            [ classList_
                [ (ms $ "hero " ++ show side, True)
                , (ms $ show $ currentAnimation hero, True)
                ]
            , animationEndHandler $ heroID hero
            ]
            []
        , p_ [class_ "text"] [text $ ms $ name hero]
        ]

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
        [buildBattleSide LeftSide m, buildBattleSide RightSide m]
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
                  , humanActive m && canCast Hack m (findHeroID LeftSide m) ||
                    battleFinished m)
                ]
            ]
            [ p_
                [ onClick $
                  if battleFinished m
                    then Restart
                    else if humanActive m && canCast Hack m (findHeroID LeftSide m)
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

buildHealthBarWidth :: Hero -> MisoString
buildHealthBarWidth hero =
  ms
    ("calc(" ++
     "var(--health-bar-width) * " ++
     "(" ++ show (health hero) ++ "/" ++ show 1000 ++ "))")

calculateHealthBarColorLabel :: Hero -> (MisoString, Bool)
calculateHealthBarColorLabel hero =
  if | health hero > 500 -> ("full", True)
     | health hero > 250 -> ("medium", True)
     | otherwise -> ("low", True)

buildFocusBarWidth :: Hero -> MisoString
buildFocusBarWidth hero =
  ms
    ("calc(" ++
     "var(--focus-bar-width) * " ++
     "(" ++ show (focusAmount hero) ++ "/" ++ show 100 ++ "))")
