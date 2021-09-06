{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module API.PolygonApi
    ( run
    , aggTrades
    , prevOHLC
    , queries
    ) where


import API.Prelude
import qualified API.Types as AT

type PolygonAPI =
  ("v2":>
    ("aggs" :> ("ticker" :> Capture "ticker" AT.Ticker
            -- aggregate OHLC Bars
                         :> "range" :> Capture "multiplier" AT.Multiplier
                         :> Capture "timespan" AT.Timespan
                         :> Capture "from" AT.FromDate
                         :> Capture "to" AT.ToDate
                         -- :> QueryParam "adjusted" AdjustSplit
                         -- :> QueryParam "sort" WillSort
                         -- :> QueryParam "limit" QueryLimit
                         :> QueryParam "apiKey" AT.ApiKey
                         :> Get '[JSON] AT.AggTrades
                    -- previous OHLC
           :<|> "ticker" :> Capture "ticker" AT.Ticker
                         :> "prev" :> QueryParam "apiKey" AT.ApiKey
                         :> Get '[JSON] AT.AggTrades)))

polygonApi :: Proxy PolygonAPI
polygonApi = Proxy

-- queries to each endpoint of api
aggTrades' :: AT.Ticker -> AT.Multiplier
          -> AT.Timespan -> AT.FromDate -> AT.ToDate 
          -> Maybe AT.ApiKey -> ClientM AT.AggTrades
prevOHLC' :: AT.Ticker -> Maybe AT.ApiKey -> ClientM AT.AggTrades
-- generate functions given the queries
aggTrades' :<|> prevOHLC' = client polygonApi


-- apiCalls which map to the above queries
aggTrades :: AT.Ticker -> AT.Multiplier -> AT.Timespan
           -> AT.FromDate -> AT.ToDate -> AT.ApiKey -> ClientM AT.AggTrades
aggTrades ticker multiplier timespan from to apiKey =
  aggTrades' ticker multiplier timespan from to (Just apiKey)

prevOHLC :: AT.Ticker -> AT.ApiKey -> ClientM AT.AggTrades
prevOHLC ticker apiKey = prevOHLC' ticker (Just apiKey)

queries :: AT.Ticker -> AT.Multiplier -> AT.Timespan -> AT.FromDate -> AT.ToDate 
        -> AT.ApiKey -> ClientM (AT.AggTrades, AT.AggTrades)
queries ticker multiplier timespan from to apiKey = do
  agg <- aggTrades' ticker multiplier timespan from to (Just apiKey)
  prev <- prevOHLC' ticker (Just apiKey)
  return (agg, prev)

{- calling the apis-}
-- runClientM, but with the ClientEnv already given
type Runner a = ClientM a -> IO a

-- TODO: have in readerT format to get the Manager
-- code: asks config
-- run :: Manager -> ClientM a -> IO (Either ClientError a)
-- run mgr apiCall = either throw return =<<
--   runClientM apiCall (mkClientEnv mgr apiPath)
--   where
--     apiPath = parseBaseUrl (unpack AT.endpoint)

run :: Manager -> ClientM a -> IO (Either ClientError a)
run mgr apiCall = do
  apiPath <- parseBaseUrl (unpack AT.endpoint)
  -- runClientM apiCall (mkClientEnv mgr apiPath) >>= return
  runClientM apiCall (mkClientEnv mgr apiPath)
  
  

{-
main :: IO ()
main = do
  manager' <- newManager tlsManagerSettings
  apiPath <- parseBaseUrl "api.polygon.io"
  res <- runClientM (queries (T.pack "X:ADAUSD") 1 (T.pack "day")
                             (T.pack "2021-07-24") (T.pack "2021-07-27")
                             (T.pack key))
                    (mkClientEnv manager' apiPath)
  -- print res
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (aggTrades, prevOHLC) -> do 
      print aggTrades
      print prevOHLC
-}