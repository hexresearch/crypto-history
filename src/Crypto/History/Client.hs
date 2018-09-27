{-# LANGUAGE CPP #-}
module Crypto.History.Client(
    getHistory
  , getAllTimeHistory
  , getCoinId
  , listCodes
) where


import Data.Aeson
import Data.Char
import Data.Either
import Data.Maybe
import Data.Proxy
import Data.Text (Text)
import Data.Time
import Network.HTTP.Client (newManager, Manager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API
import Servant.API.ContentTypes
import Servant.HTML.Blaze
import Servant.Client

import Crypto.History.Types
import Crypto.History.Parse

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LB
import qualified Data.List as L
import qualified Data.HashMap.Lazy as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

-- | Fetches historical data for cryptocurrency with given three letter code
-- in the given time frame. If both times ommited it fetches for all time.
--
-- > gethistory startMay endMay currency = ...
--
-- example:
--
-- > getHistory Nothing Nothing "eth"
getHistory :: Maybe UTCTime -> Maybe UTCTime -> CoinId -> IO (Either Text History)
getHistory mstart mend (CoinId coinId) = do
  now <- getCurrentTime
  ehtml <- history coinId (Just $ withDefaultStart mstart) (Just $ withDefaultEnd now mend)
  return $ case ehtml of
    Right html -> case getTable html of
      Just hist -> Right hist
      Nothing   -> Left "Failed to parse table data from HTML."
    Left msg   -> Left msg
  where
    withDefaultEnd :: UTCTime -> Maybe UTCTime -> Text
    withDefaultEnd now = toTextTime . maybe (toYesturday now) id

    withDefaultStart :: Maybe UTCTime -> Text
    withDefaultStart = toTextTime . maybe epochStart id

    toYesturday date = date { utctDay = addDays (-1) $ utctDay date }
    epochStart = fromJust $ parseTimeM True defaultTimeLocale "%F" "2013-01-01"

    toTextTime = T.pack . formatTime defaultTimeLocale "%Y%m%d"

getTable :: Text -> Maybe History
getTable txt
  | V.null hist = Nothing
  | otherwise   = Just hist
  where hist = parseHistory $ T.unpack txt

-- | Fetches all time history data for cryptocurrency with given three letter code.
getAllTimeHistory :: CoinId -> IO (Either Text History)
getAllTimeHistory = getHistory Nothing Nothing

-- | Lists all supported cryptocurrency codes.
listCodes :: IO [Text]
listCodes = fmap (fmap (unCoinCode . coinData'symbol) . concat) $ go 1
  where
    go start = do
      xs <- listCodesAndIdBy start
      case xs of
        [] -> do
            putStrLn "Done"
            return []
        as -> do
            putStrLn $ mconcat ["Loaded so far: ", show start]
            fmap (as : ) $ go (start + 100)

data CoinData = CoinData
  { coinData'symbol       :: CoinCode
  , coinData'website_slug :: CoinId
  } deriving (Show, Eq)

listCodesAndIdBy :: Int -> IO [CoinData]
listCodesAndIdBy start = do
  eResp <- ticker (Just "id") (Just start) (Just 100)
  return $ fromRight [] $ parseIds =<< eResp
  where
    parseIds val = case val of
      A.Object obj -> case M.lookup "data" obj of
        Just (A.Object curs) -> mapM getSymbol $ M.elems curs
        _                    -> failedToParse

    getSymbol val = case val of
      A.Object obj -> case (M.lookup "symbol" obj, M.lookup "website_slug" obj) of
          (Just (A.String sym), Just (A.String slug)) -> pure $ CoinData (CoinCode sym) (CoinId slug)
          _                                           -> failedToParse

    failedToParse = Left "Failed to parse list of currency tickers"

-- | Gets the coin-id by coin code.
-- The coin-ids are used internally by coinmarketcap.com to distinguish historical data.
-- For example we can query the coin-id for ETH:
--
-- > Right coinId <- getCoinId (CoinCode "ETH")
-- > coinId
-- > CoinId "ethereum"
getCoinId :: CoinCode -> IO (Maybe CoinId)
getCoinId (CoinCode sym) = findSlug 1
  where
    findSlug start = do
      xs <- listCodesAndIdBy start
      case xs of
        [] -> do
            return Nothing
        as -> do
            case L.find ((== normSym) . coinData'symbol) as of
              Just res -> return $ Just $ coinData'website_slug res
              Nothing  -> findSlug (start + 100)

    normSym = CoinCode $ T.map toUpper sym

---------------------------
-- API

type CoinMarketAPI =
       TickerEndpoint
  :<|> HistoryEndpoint

type TickerEndpoint = "v2" :> "ticker"
  :> QueryParam "sort" Text
  :> QueryParam "start" Int
  :> QueryParam "limit" Int
  :> Get '[JSON] Value

type HistoryEndpoint = "currencies"
  :> Capture "currency-id" Text
  :> "historical-data"
  :> QueryParam "start" Text
  :> QueryParam "end"   Text
  :> Get '[HTML] Text

---------------------------
-- client

ticker :: Maybe Text -> Maybe Int -> Maybe Int -> IO (Either Text Value)
ticker sort start end = run (ticker' sort start end)

history :: Text -> Maybe Text -> Maybe Text -> IO (Either Text Text)
history coinId start end = runPage (history' coinId start end)

ticker' :: Maybe Text -> Maybe Int -> Maybe Int -> ClientM Value
history' :: Text -> Maybe Text -> Maybe Text -> ClientM Text

ticker' :<|> history' = client (Proxy :: Proxy CoinMarketAPI)

makeEnv :: Manager -> String -> ClientEnv
makeEnv manager url =
#if MIN_VERSION_servant_client(0,14,0)
  ClientEnv manager (BaseUrl Https url 443 "") Nothing
#else
  ClientEnv manager (BaseUrl Https url 443 "")
#endif

run :: ClientM a -> IO (Either Text a)
run go = do
  manager' <- newManager tlsManagerSettings
  res <- runClientM go $ makeEnv manager' "api.coinmarketcap.com"
  return $ either (Left . T.pack . show) Right res

runPage :: ClientM a -> IO (Either Text a)
runPage go = do
  manager' <- newManager tlsManagerSettings
  res <- runClientM go $ makeEnv manager' "coinmarketcap.com"
  return $ either (Left . T.pack . show) Right res

--------------------------

instance MimeUnrender HTML Text where
  mimeUnrender _ bs = Right $ T.decodeUtf8 $ LB.toStrict bs
