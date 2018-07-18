# crypto-history

Fetches historical data for cryptocurrencies from [coinmarketcap.com](https://coinmarketcap.com/).


With this library we can load and save historical data for various crypto-currencies.
It uses API to [coinmarketcap.com](https://coinmarketcap.com/) to fetch the data.

In the coinmarketcap the currencies are referenced by special ids.
we can query the specific id with the function `getCoinId`:

```haskell
> import Crypto.History
> Right coinId <- getCoinId (CoinCode "ETH")
> print coinId
> CoinId "ethereum"
```

Once we know the id we can query the historical data:

```haskell
> Right hist <- getHistory (dateStr "01-01-2015") (dateStr "01-01-2018") coinId
```

The `dateStr` is a helper function to quickly construct `UTCTime` from string in the format `dd-mm-yyyy`.
Also we can query all time history data:

```haskell
> Right hist <- getAllTimeHistory coinId
```

All prices are relative to USD. But if we want to relate to another currency we can
use a special variant that uses cross-prices to calculate the given relation:

```haskell
> Right hist <- getHistory2 (dateStr "01-01-2015") (dateStr "01-01-2018") (coinIdA, coinIdB)
```

It uses the function `cross :: History -> History -> History` under the hood
to create cross-prices relative to specific coins.

We can save and load the data to/from CSV files:

```haskell
> writeHistory "file.csv" hist
> Just hist <- readHistory "file.csv"
```



