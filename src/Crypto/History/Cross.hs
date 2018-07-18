-- | Makes cross history
--
-- By default all history-reports are relative to USD.
-- If we want to find out data relative to other courses we can use this module.
module Crypto.History.Cross(
    cross
  -- * Client
  , getHistory2
  , getAllTimeHistory2
) where

import Control.Applicative
import Data.Ord
import Data.Text (Text)
import Crypto.History.Client
import Crypto.History.Types

import qualified Data.Vector as V

-- | Creates cross-history from two histories.
-- By default all loaded crypto histories are relative to USD.
-- If we want to relate to other coins we can use this function.
cross :: History -> History -> History
cross histA histB = V.fromList $ zipSame (comparing frame'date) relateFrame (V.toList histA) (V.toList histB)

-- | It implies that items are ordered by cmp from greatest to lowest.
-- Which is what we get by reading history with @getHistory@.
zipSame :: (a -> a -> Ordering) -> (a -> a -> a) -> [a] -> [a] -> [a]
zipSame cmp f xs ys = case (xs, ys) of
  ([], _) -> []
  (_, []) -> []
  (a:as, b:bs) -> case cmp a b of
    EQ -> f a b : rec as bs
    LT -> rec (a:as) bs
    GT -> rec as (b:bs)
  where rec = zipSame cmp f

relateFrame :: Frame -> Frame -> Frame
relateFrame (Frame dateA openA highA lowA closeA volumeA marketCapA) (Frame dateB openB highB lowB closeB volumeB marketCapB) =
    Frame dateA (openA / openB) (highA / highB) (lowA / lowB) (closeA / closeB) volumeA marketCapA

----------------------------------
-- client liftings

-- | Loads cross history of two coins relative to each other.
getHistory2 :: Maybe UTCTime -> Maybe UTCTime -> (CoinId, CoinId) -> IO (Either Text History)
getHistory2 start end (coinA, coinB) = do
  histA <- getHistory start end coinA
  histB <- getHistory start end coinB
  return $ liftA2 cross histA histB

-- | Loads cross history of two coins relative to each other for all history of both coins.
getAllTimeHistory2 :: (CoinId, CoinId) -> IO (Either Text History)
getAllTimeHistory2 = getHistory2 Nothing Nothing
