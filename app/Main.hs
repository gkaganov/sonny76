{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

module Main
  ( main
  ) where

import Model
import Update
import View

import Miso

import Data.Function ((&))

import qualified Data.Map as M
--
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
  runApp :: IO () -> IO ()
  runApp app = app
#endif
-- miso entrypoint
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
