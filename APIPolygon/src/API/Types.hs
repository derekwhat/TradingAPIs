{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}


module API.Types
    ( endpoint
    , AggTrades
    , Ticker
    , Multiplier
    , Timespan
    , FromDate
    , ToDate 
    , ApiKey
    ) where

import qualified Data.Text  as T
import API.Prelude


type Ticker = T.Text
type Multiplier = Int
type Timespan = T.Text
type FromDate = T.Text
type ToDate = T.Text
type ApiKey = T.Text

endpoint :: T.Text
endpoint = "api.polygon.io"

data OHLCbar = OHLCbar 
  { v :: Double     -- volume
  , vw :: Double    -- VWAP
  , o :: Double     -- open
  , c :: Double     -- close
  , h :: Double     -- highest
  , l :: Double     -- lowest
  , t :: Integer    -- start time in Unix Msec
  , n :: Int        -- number of trades
  }
  deriving (Show, Generic, FromJSON)

type OHLCbars = [OHLCbar]

data AggTrades = AggTrades 
  { status :: String
  , ticker :: String
  , queryCount :: Int
  , resultsCount :: Int -- should equal querycount for complete result
  , adjusted :: Bool
  , results :: OHLCbars
  }
  deriving (Show, Generic, FromJSON)
