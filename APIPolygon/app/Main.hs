-- stack --resolver lts-13.7 script

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

import System.IO 
import Data.Text                  (pack)
import Control.Monad.IO.Class     (liftIO)
import Network.HTTP.Client        (newManager)
import Network.HTTP.Client.TLS    (tlsManagerSettings)

import Servant.Client             (mkClientEnv, runClientM, parseBaseUrl)

import System.Environment ()
-- for handling array of results
import qualified Data.Vector as V

import API.PolygonApi


-- https://api.polygon.io/v2/aggs/ticker/X:ADAUSD/range/1/day/2021-07-24/2021-07-27?apiKey=O5ZnLHNhwqPLC_EkdspvZxFPGRPhxtI8

-- maybe make it auto refresh/get new key automatically
keyPath =  "../../APIkeys/PolygonAPI.txt"

-- TODO: make it so can feed key at any time
main :: IO ()
main = do
  key <- hGetLine =<< openFile keyPath ReadMode
  manager' <- newManager tlsManagerSettings
  liftIO . print =<< (run manager' $ aggTrades (pack "X:ADAUSD") 1 (pack "day") (pack "2021-07-24") (pack "2021-07-27") (pack key))
  liftIO . print =<< (run manager' $ prevOHLC (pack "X:ADAUSD") (pack key))
                    