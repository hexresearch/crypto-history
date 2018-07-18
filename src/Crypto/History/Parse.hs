module Crypto.History.Parse(
  parseHistory
) where

import Data.Fixed
import Data.Text (Text)
import Data.Time
import Data.Maybe
import Text.HTML.TagSoup
import Text.StringLike
import Safe

import Crypto.History.Types
import qualified Data.Text as T
import qualified Data.Vector as V

txt :: String -> String
txt = id

getRows :: [Tag String] -> [[Tag String]]
getRows = fmap (takeWhile (~/= (txt "</tr>")) . drop 1) . sections (~== (txt "<tr class=\"text-right\">"))

extractData :: [Tag String] -> Maybe Frame
extractData row = do
  frame'date <- getDate row
  (frame'open, frame'high, frame'low, frame'close, frame'volume, frame'marketCap) <- getNums row
  return $ Frame{..}

parseHistory :: String -> History
parseHistory = V.fromList . catMaybes . map extractData . getRows . parseTags

getDate :: [Tag String] -> Maybe UTCTime
getDate = parseTimeM True defaultTimeLocale "%b %d, %Y" . innerText . takeWhile (~/= (txt "</td>")) . drop 1 . dropWhile (~/= (txt "<td>"))

getNums :: [Tag String] -> Maybe (Pico, Pico, Pico, Pico, Pico, Pico)
getNums row = case extract row of
  [Just a, Just b, Just c, Just d, Just e, Just f] -> Just (a, b, c, d, e, f)
  _                                                -> Nothing
  where
    extract :: [Tag String] -> [Maybe Pico]
    extract x = fmap (rnum . fromAttrib "data-format-value") $ drop 1 $ fmap head $ fmap (takeWhile (~/= (txt "</td>"))) $  sections (~== (txt "<td>")) x

    rnum :: String -> Maybe Pico
    rnum = readMay
