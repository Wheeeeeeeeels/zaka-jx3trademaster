module Analyzer.TradeAnalyzer where

import Control.Monad
import Data.List
import Data.Time
import Model.Item
import Data.Maybe

class Monad m => Analyzer m where
    analyzePriceTrend :: Int -> [PriceRecord] -> m PriceTrend
    generateTradeAdvice :: PriceTrend -> m TradeAdvice

data AnalyzerError = InsufficientData String
                   | AnalysisError String
    deriving (Show)

newtype SimpleAnalyzer m = SimpleAnalyzer {
    minDataPoints :: Int
}

instance Monad m => Analyzer (SimpleAnalyzer m) where
    analyzePriceTrend itemId records = do
        if length records < minDataPoints (SimpleAnalyzer 5)
            then error "数据点不足"
            else return $ calculateTrend itemId records

    generateTradeAdvice trend = do
        return $ generateAdvice trend

calculateTrend :: Int -> [PriceRecord] -> PriceTrend
calculateTrend itemId records = 
    let sortedRecords = sortBy (\a b -> compare (timestamp a) (timestamp b)) records
        currentPrice = price $ last sortedRecords
        averagePrice = sum (map price records) / fromIntegral (length records)
        priceChange = currentPrice - averagePrice
        trendDir = if priceChange > 0 then Up else if priceChange < 0 then Down else Stable
    in PriceTrend {
        itemId = itemId,
        currentPrice = currentPrice,
        averagePrice = averagePrice,
        priceChange = priceChange,
        trend = trendDir
    }

generateAdvice :: PriceTrend -> TradeAdvice
generateAdvice trend = 
    let (advice, confidence, risk) = case (trend trend, abs (priceChange trend)) of
            (Up, change) | change > averagePrice trend * 0.2 -> 
                ("价格显著上涨，建议观望", 0.8, High)
            (Up, _) -> 
                ("价格温和上涨，可考虑买入", 0.6, Medium)
            (Down, change) | change > averagePrice trend * 0.2 -> 
                ("价格大幅下跌，可考虑买入", 0.7, Medium)
            (Down, _) -> 
                ("价格温和下跌，建议观望", 0.6, Medium)
            (Stable, _) -> 
                ("价格稳定，可考虑长期持有", 0.5, Low)
    in TradeAdvice {
        itemId = itemId trend,
        advice = advice,
        confidence = confidence,
        riskLevel = risk
    } 