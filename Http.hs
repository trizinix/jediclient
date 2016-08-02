{-# LANGUAGE OverloadedStrings #-}

module Http where

import Prelude
import Reflex
import Reflex.Dom hiding (list)
import GHCJS.DOM.XMLHttpRequest (abort)
import Data.Aeson
import Control.Monad (join)
import Data.Maybe
import Data.List (nub, sort)

import Types
import Util
import Model

apiPath = "http://localhost:3000"
darthSidiousId = 3616

missingSiths :: State -> [JediLink]
missingSiths state = if thereIsAMatch state then [] else sithsNotInList
  where siths = list state
        masters = fmap master $ catMaybes $ tail siths
        apprentices = fmap apprentice $ catMaybes $ tail (reverse siths)
        allSiths = nub $ sort $ filter isValidLink $ masters ++ apprentices
        sithsNotInList = filter (flip jediNotElem $ catMaybes siths) allSiths

aMissingSith :: State -> Maybe JediLink
aMissingSith = listToMaybe . missingSiths

http :: MonadWidget t m => Dynamic t State -> m (Event t Jedi)
http state = do
  initialEvent <- getPostBuild
  let initialRequest = ffor initialEvent $ \_ -> apiPath ++ "/dark-jedis/" ++ show darthSidiousId
  let distinctState = updated state
  let sithsToFetch = fmapMaybe (join . fmap linkUrl .  aMissingSith) distinctState
  let allSithsToFetch = leftmost [initialRequest, sithsToFetch]
  let xhrRequests = ffor allSithsToFetch $ \url -> xhrRequest "GET" url def
  responses <- performRequestAsync xhrRequests
  let fetchedSith = fmap (fromJust . decodeXhrResponse) responses
  return fetchedSith
