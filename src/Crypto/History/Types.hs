module Crypto.History.Types(
    History
  , Frame(..)
) where

import Control.Monad
import Data.Scientific
import Data.Csv
import Data.Text (Text)
import Data.Time
import Data.Vector (Vector, (!))

import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T

type History = Vector Frame

-- | Single crypto-currency frame.
data Frame = Frame
  { frame'date      :: UTCTime
  , frame'open      :: Scientific
  , frame'high      :: Scientific
  , frame'low       :: Scientific
  , frame'close     :: Scientific
  , frame'volume    :: Scientific
  , frame'marketCap :: Scientific
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

instance FromRecord Frame where
  parseRecord v = maybe mzero pure . fromTuple =<< parseRecord v
    where
      fromTuple :: (Text, Scientific, Scientific, Scientific, Scientific, Scientific, Scientific) -> Maybe Frame
      fromTuple (dateTxt, frame'open, frame'high, frame'low, frame'close, frame'volume, frame'marketCap) =
        fmap (\frame'date -> Frame{..}) $ parseFrameData dateTxt

parseFrameData :: Text -> Maybe UTCTime
parseFrameData = parseTimeM True defaultTimeLocale "%d-%m-%Y" . T.unpack

printFrameData :: UTCTime -> Text
printFrameData = T.pack . formatTime defaultTimeLocale "%d-%m-%Y"
