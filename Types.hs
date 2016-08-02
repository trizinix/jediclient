{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Prelude hiding (id)
import GHC.Generics
import Data.Default
import Data.Aeson
import Data.Aeson.Types hiding (id)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

data Planet = Planet { planetId :: Int
                     , planetName :: String
                     }
              deriving (Generic, Show, Eq)

instance FromJSON Planet where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 7 }


data Jedi = Jedi { id :: Int
                 , name :: String
                 , homeworld :: Planet
                 , matched :: Bool
                 , apprentice :: JediLink
                 , master :: JediLink
                 }
            deriving (Generic, Show)

instance FromJSON Jedi where
  parseJSON (Object v) =
    Jedi <$> v .: "id"
         <*> v .: "name" .!= ""
         <*> v .: "homeworld" .!= Planet 0 ""
         <*> v .:? "matched" .!= False
         <*> v .: "apprentice"
         <*> v .: "master"

instance Eq Jedi where
  (Jedi i1 _ _ m1 _ _) == (Jedi i2 _ _ m2 _ _) = (i1, m1) == (i2, m2)

data JediLink = JediLink { linkId :: Maybe Int
                         , linkUrl :: Maybe String
                         }
              deriving (Generic, Show)


instance FromJSON JediLink where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 5 }

instance Eq JediLink where
  j1 == j2 = linkId j1 == linkId j2

instance Ord JediLink where
  compare j1 j2 = compare (linkId j1) (linkId j2)


isValidLink :: JediLink -> Bool
isValidLink (JediLink Nothing _) = False
isValidLink _ = True

jediElemIndex :: Jedi -> [Maybe JediLink] -> Maybe Int
jediElemIndex jedi = elemIndex (Just link)
  where link = JediLink (Just $ id jedi) Nothing
  
jediNotElem :: JediLink -> [Jedi] -> Bool
jediNotElem x xs = i `notElem` ids
  where i = fromJust $ linkId x
        ids = map id xs
