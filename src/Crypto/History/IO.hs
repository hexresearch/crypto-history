-- | Saves and reads the history as csv files
module Crypto.History.IO(
    readHistory
  , readHistoryLazy
  , writeHistory
) where

import Data.Csv
import Data.Either

import Crypto.History.Types

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB
import qualified Data.Vector as V

-- | Saves crypto history as CSV-file.
writeHistory :: String -> History -> IO ()
writeHistory file hist = LB.writeFile file $ encodeByName header $ V.toList hist
  where header = V.fromList ["Date", "Open*", "High", "Low", "Close**", "Volume", "Market Cap"]

-- | Reads crypto history from CSV-file.
readHistoryLazy :: String -> IO (Maybe History)
readHistoryLazy file = fmap (fromRight Nothing . fmap Just . decode HasHeader) $ LB.readFile file

-- | Reads crypto history from CSV-file.
readHistory :: String -> IO (Maybe History)
readHistory file = fmap (fromRight Nothing . fmap Just . decode HasHeader . LB.fromStrict) $ BS.readFile file
