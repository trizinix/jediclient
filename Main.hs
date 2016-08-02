{-# LANGUAGE RecursiveDo, OverloadedStrings#-}

import Reflex
import Reflex.Dom hiding (list)
import GHCJS.DOM.XMLHttpRequest (abort)
import qualified Data.Map as Map
import qualified Data.List.NonEmpty as NonEmpty
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Maybe

import Types
import Util
import Model
import Http
import Action
import View

main :: IO ()
main = mainWidgetWithHead headElement bodyElement

headElement :: MonadWidget t m => m ()
headElement = do
  el "title" (text "Reflex Flux Challenge")
  styleSheet "css/style.css"
  where
    styleSheet link = elAttr "link" (Map.fromList [
          ("rel", "stylesheet")
        , ("type", "text/css")
        , ("href", link)
      ]) $ return ()

bodyElement :: MonadWidget t m => m ()
bodyElement = el "div" $
  elClass "div" "app-container" $
    elClass "div" "css-root"
      mainElement


title :: MonadWidget t m => Dynamic t State -> m ()
title state = elClass "h1" "css-planet-monitor" $ do
  planet <- mapDyn ((intro ++) . planetName . planet) state
  dynText planet
  where
    intro = "Obi-Wan currently on "

mainElement :: MonadWidget t m => m ()
mainElement = do
  rec
    planet <- obiwanPlanet
    response <- http state
    
    state <- model planet scroll response
    title state
    scroll <- view state
  return ()
