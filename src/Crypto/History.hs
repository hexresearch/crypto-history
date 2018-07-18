-- | Load and save historical data for various crypto-currencies.
-- It uses API to <coinmarketcap.com> to fetch the data.
--
-- In the coinmarketcap the currencies are referenced by special ids.
-- we can query the specific id with the function @getCoinId@:
--
-- > Right coinId <- getCoinId (CoinCode "ETH")
-- > print coinId
-- > CoinId "ethereum"
--
-- Once we know the id we can query the historical data:
--
-- > Right hist <- getHistory (dateStr "01-01-2015") (dateStr "01-01-2018") coinId
--
-- The @dateStr@ is a helper function to quickly construct @UTCTime@ from string.
-- Also we can query all time history data:
--
-- > Right hist <- getAllTimeHistory coinId
--
-- All prices are relative to USD. But if we want to relate to another currency we can
-- use a special variant that uses cross-prices to calculate the given relation:
--
-- > Right hist <- getHistory2 (dateStr "01-01-2015") (dateStr "01-01-2018") (coinIdA, coinIdB)
--
-- We can save and load the data to/from CSV files:
--
-- > writeHistory "file.csv" hist
-- > Just hist <- readHistory "file.csv"
module Crypto.History(
  -- * Types
    module Crypto.History.Types

  -- * Client
  , module Crypto.History.Client

  -- * IO
  , module Crypto.History.IO

  -- * Cross
  , module Crypto.History.Cross

  -- * Utils
  , module Crypto.History.Utils
) where

import Crypto.History.Client
import Crypto.History.Cross
import Crypto.History.IO
import Crypto.History.Types
import Crypto.History.Utils

