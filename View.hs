{-# LANGUAGE OverloadedStrings #-}

module View where

import Prelude hiding (head)
import Reflex
import Reflex.Dom hiding (list)
import GHCJS.DOM.XMLHttpRequest (abort)
import qualified Data.Map as Map
import qualified Data.List.NonEmpty as NonEmpty
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import Control.Applicative

import Types
import Util
import Model

view :: MonadWidget t m => Dynamic t State -> m (Event t Int)
view state = elClass "section" "css-scrollable-list" $ do
  renderList state
  scrollButtons state

renderList :: MonadWidget t m => Dynamic t State -> m ()
renderList state = do
  siths <- mapDyn list state
  elClass "ul" "css-slots" $
    simpleList siths $ \sith -> do
      let liClass = Map.singleton "class" "css-slot" :: AttributeMap
      cssStyleDynamic <- mapDyn (Map.union liClass . colorMatch) sith
      elDynAttr "li" cssStyleDynamic $ do
        el "h3" $ dynText =<< mapDyn (maybe "" name) sith
        el "h6" $ dynText =<< mapDyn (maybe "" (planetName . homeworld)) sith
  return ()


colorMatch :: Maybe Jedi -> AttributeMap
colorMatch (Just sith) = if matched sith
  then Map.singleton "style" "color: red"
  else Map.singleton "style" ""
colorMatch Nothing = Map.empty

buttonClass :: MonadWidget t m => String -> Dynamic t Bool -> m (Event t ())
buttonClass elClass disabled = do
  activeClass <- forDyn disabled $ \x -> if x then "css-button-disabled " else ""
  cssClassDynamic <- forDyn activeClass $ \x -> Map.singleton "class" (x ++ elClass)
  (button, _) <- elDynAttr' "button" cssClassDynamic $ return ()
  return $ domEvent Click button


isBlockedBy :: (State -> Bool) -> State -> Bool
isBlockedBy borderHasLink = liftA2 (||) thereIsAMatch (not . borderHasLink)

scrollButtons :: MonadWidget t m => Dynamic t State -> m (Event t Int)
scrollButtons state = divClass "css-scroll-buttons" $ do
  upBlocked <- mapDyn (isBlockedBy firstSithHasMaster) state
  downBlocked <- mapDyn (isBlockedBy lastSithHasApprentice) state
  upEvent <- buttonClass "css-button-up" upBlocked
  downEvent <- buttonClass "css-button-down" downBlocked
  let up = fmap (const (2 :: Int)) upEvent
  let down = fmap (const (-2 :: Int)) downEvent
  return $ leftmost [up, down]
