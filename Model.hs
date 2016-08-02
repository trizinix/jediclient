{-# LANGUAGE DeriveAnyClass, OverloadedStrings #-}

module Model where

import Data.Maybe
import Reflex
import Reflex.Dom hiding (list)
import Control.Monad (join)
import Data.List (elemIndex, replicate)

import Types
import Util

data State = State { planet :: Planet
                   , list :: [Maybe Jedi]
                   }
             deriving (Eq)

initialState = State (Planet 0 "") (replicate 5 Nothing)

thereIsAMatch :: State -> Bool
thereIsAMatch state = any isMatched $ list state
  where isMatched Nothing = False
        isMatched (Just x) = matched x


firstSithHasMaster :: State -> Bool
firstSithHasMaster state = firstValid valids
  where valids = catMaybes $ list state
        firstValid (jedi:xs) = isValidLink $ master jedi
        firstValid _ = False

lastSithHasApprentice :: State -> Bool
lastSithHasApprentice state = firstValid valids
  where valids = (catMaybes . reverse) $ list state
        firstValid (jedi:xs) = isValidLink $ apprentice jedi
        firstValid _ = False


data Command
  = UpdateWithNewSith Jedi
  | UpdateWithNewPlanet Planet
  | UpdateWhenScrolled Int
  deriving (Show)

calculateHomeworldMatches :: Planet -> [Maybe Jedi] -> [Maybe Jedi]
calculateHomeworldMatches planet = map helper
  where
    helper (Just jedi) = if homeworld jedi == planet
                         then Just $ jedi { matched = True }
                         else Just $ jedi { matched = False }
    helper Nothing = Nothing

replaceAtIndex n item xs = take n xs ++ [item] ++ drop (n+1) xs

fitNewSithIntoList :: Jedi -> [Maybe Jedi] -> [Maybe Jedi]
fitNewSithIntoList newSith list
  | isJust indexAsMaster = replaceInList $ -1 + fromJust indexAsMaster
  | isJust indexAsApprentice = replaceInList $ 1 + fromJust indexAsApprentice
  | all isNothing list = replaceInList (length list `quot` 2)
  | otherwise = list
  where
    replaceInList idx = replaceAtIndex idx (Just newSith) list
    masters = map (fmap master) list
    apprentices = map (fmap apprentice) list
    indexAsMaster = jediElemIndex newSith masters
    indexAsApprentice = jediElemIndex newSith apprentices


updateState :: Command -> State -> State
updateState (UpdateWithNewPlanet newPlanet) state =
  let updatedList = calculateHomeworldMatches newPlanet (list state)
  in State newPlanet updatedList
updateState (UpdateWithNewSith jedi) state =
  let insertedList = calculateHomeworldMatches (planet state) $ fitNewSithIntoList jedi (list state)
  in state { list = insertedList }
updateState (UpdateWhenScrolled delta) state
  | thereIsAMatch state = state
  | delta > 0 && firstSithHasMaster state = state { list = shiftBack }
  | delta < 0 && lastSithHasApprentice state = state { list = shiftFront }
  | otherwise = state
      where replacement = [Nothing, Nothing]
            shiftFront = drop 2 (list state) ++ replacement
            shiftBack =  replacement ++ dropLast 2 (list state)


model :: MonadWidget t m => Event t Planet -> Event t Int -> Event t Jedi -> m (Dynamic t State)
model planet scrollBy newSith = do
  let updatePlanetEvent = fmap UpdateWithNewPlanet planet
  let updateScrollEvent = fmap UpdateWhenScrolled scrollBy
  let updateNewSithEvent = fmap UpdateWithNewSith newSith
  let updateEvents = leftmost[updatePlanetEvent, updateScrollEvent, updateNewSithEvent]
  state <- foldDyn updateState initialState updateEvents
  return state
