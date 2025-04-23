{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Model.Item where

import Data.Aeson
import GHC.Generics
import Data.Time

data Quality = Common | Uncommon | Rare | Epic | Legendary
    deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data Item = Item {
    itemId :: Int,
    name :: String,
    category :: String,
    subCategory :: String,
    quality :: Quality,
    level :: Int
} deriving (Show, Eq, Generic, ToJSON, FromJSON)

data PriceRecord = PriceRecord {
    itemId :: Int,
    price :: Double,
    timestamp :: UTCTime,
    server :: String,
    quantity :: Int
} deriving (Show, Eq, Generic, ToJSON, FromJSON)

data PriceTrend = PriceTrend {
    itemId :: Int,
    currentPrice :: Double,
    averagePrice :: Double,
    priceChange :: Double,
    trend :: TrendDirection
} deriving (Show, Eq, Generic, ToJSON, FromJSON)

data TrendDirection = Up | Down | Stable
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

data TradeAdvice = TradeAdvice {
    itemId :: Int,
    advice :: String,
    confidence :: Double,
    riskLevel :: RiskLevel
} deriving (Show, Eq, Generic, ToJSON, FromJSON)

data RiskLevel = Low | Medium | High
    deriving (Show, Eq, Generic, ToJSON, FromJSON) 