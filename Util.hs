{-# LANGUAGE ScopedTypeVariables #-}

module Util where

import Reflex
import Reflex.Dom


dropLast :: Int -> [a] -> [a]
dropLast n = reverse . drop n . reverse

elClass' :: forall t m a. MonadWidget t m => String -> String -> m a -> m (El t, a)
elClass' elementTag c = elWith' elementTag $ def & attributes .~ "class" =: c
