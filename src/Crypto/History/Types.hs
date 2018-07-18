module Crypto.History.Types(
    CoinId(..)
  , CoinCode(..)
  , History
  , Frame(..)
  , module Data.Time
) where

import Control.Monad
import Data.Fixed
import Data.Scientific
import Data.Csv
import Data.String
import Data.Text (Text)
import Data.Time
import Data.Vector (Vector, (!))

import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T

-- | Internal for coinmarket.com name that reperesents the coin.
-- We can get the internal name by coin code by function @getCoinCode@.
newtype CoinId   = CoinId   { unCoinId :: Text
  } deriving (Show, Eq)

-- | Short text code that represents the coin: ETH, BTC, XRP
-- We can get the list of all supported codes with function @listCodes@
newtype CoinCode = CoinCode { unCoinCode :: Text
  } deriving (Show, Eq)

instance IsString CoinCode where
  fromString = CoinCode . T.pack

instance IsString CoinId where
  fromString = CoinId . T.pack

-- | All tickers one per day for a given time.
type History = Vector Frame

-- | Single crypto-currency frame.
data Frame = Frame
  { frame'date      :: UTCTime
  , frame'open      :: Pico
  , frame'high      :: Pico
  , frame'low       :: Pico
  , frame'close     :: Pico
  , frame'volume    :: Pico
  , frame'marketCap :: Pico
  } deriving (Show, Eq)

instance ToNamedRecord Frame where
  toNamedRecord Frame{..} = namedRecord
    [ "Date"        .= printFrameData frame'date
    , "Open*"       .= frame'open
    , "High"        .= frame'high
    , "Low"         .= frame'low
    , "Close**"     .= frame'close
    , "Volume"      .= frame'volume
    , "Market Cap"  .= frame'marketCap]

instance FromField Pico where
  parseField = fmap fromScientific . parseField
    where
      fromScientific :: Scientific -> Pico
      fromScientific = realToFrac

instance ToField Pico where
  toField = toField . toScientific
    where
      toScientific :: Pico -> Scientific
      toScientific = realToFrac

instance FromRecord Frame where
  parseRecord v = maybe mzero pure . fromTuple =<< parseRecord v
    where
      fromTuple :: (Text, Pico, Pico, Pico, Pico, Pico, Pico) -> Maybe Frame
      fromTuple (dateTxt, frame'open, frame'high, frame'low, frame'close, frame'volume, frame'marketCap) =
        fmap (\frame'date -> Frame{..}) $ parseFrameData dateTxt

parseFrameData :: Text -> Maybe UTCTime
parseFrameData = parseTimeM True defaultTimeLocale "%d-%m-%Y" . T.unpack

printFrameData :: UTCTime -> Text
printFrameData = T.pack . formatTime defaultTimeLocale "%d-%m-%Y"
