{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module API.Prelude
    ( module X
    ) where

import Control.Exception                        as X
    (throw)
import Data.Text                                as X
    (pack, unpack)
import GHC.Generics                             as X 
    ( Generic )
import Data.Aeson                               as X
    ( FromJSON
    , defaultOptions, genericParseJSON
    , fieldLabelModifier, parseJSON
    , withObject, (.:)
    )
import Data.Proxy                               as X
    ( Proxy(..) )
import Servant.API                              as X
    ( JSON, QueryParam, Get, Capture
    , type (:>), type (:<|>)((:<|>)))
import Servant.Client                           as X
    ( ClientM, ClientError
    , client, mkClientEnv, runClientM
    , parseBaseUrl
    )
import Network.HTTP.Client                      as X
    (Manager)


