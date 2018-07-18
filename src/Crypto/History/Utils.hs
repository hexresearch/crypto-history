module Crypto.History.Utils(
  dateStr
) where

import Data.Time

-- | Useful to quickly construct date from strings.
--
-- > dateStr "20-09-2015"
-- >
-- > Right hist <- getHistory (dateStr "01-01-2015") (dateStr "01-01-2017") (CoinId "ethereum")
dateStr :: String -> Maybe UTCTime
dateStr = parseTimeM True defaultTimeLocale "%d-%m-%Y"
