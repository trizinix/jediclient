module Action where

import Reflex
import Reflex.Dom
import Data.Maybe (fromMaybe)
import Data.Aeson
import Control.Applicative ((<$>))
import qualified Data.ByteString.Lazy as BL

import Types


obiwanPlanet :: MonadWidget t m => m (Event t Planet)
obiwanPlanet = do
  ws <- webSocket "ws://localhost:4000" def
  let helper = fromMaybe (Planet 0 "") . decode . BL.fromChunks . (:[])
  return $ helper <$> _webSocket_recv ws

