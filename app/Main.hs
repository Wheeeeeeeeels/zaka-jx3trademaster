{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import Control.Monad
import Data.Text (pack, unpack)
import Model.Item
import Collector.DataCollector
import Analyzer.TradeAnalyzer
import Storage.Database
import Data.Time
import System.Environment

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = do
    -- 设置窗口标题
    UI.setTitle window "剑网三交易助手"

    -- 创建主界面元素
    title <- UI.h1 # set text "剑网三交易助手"
    searchBox <- UI.input # set UI.type_ "text" # set UI.placeholder "输入物品ID或名称"
    searchButton <- UI.button # set text "搜索"
    resultArea <- UI.div # set UI.id_ "resultArea"

    -- 添加元素到界面
    getBody window #+ [element title, element searchBox, element searchButton, element resultArea]

    -- 设置搜索按钮事件
    on UI.click searchButton $ const $ do
        itemIdText <- get value searchBox
        case reads itemIdText of
            [(itemId, "")] -> do
                -- 模拟数据采集和分析
                now <- liftIO getCurrentTime
                let mockRecords = [
                        PriceRecord {
                            itemId = itemId,
                            price = 1000.0,
                            timestamp = now,
                            server = "电信一区",
                            quantity = 1
                        },
                        PriceRecord {
                            itemId = itemId,
                            price = 1200.0,
                            timestamp = addUTCTime (-3600) now,
                            server = "电信一区",
                            quantity = 1
                        }
                    ]
                let trend = calculateTrend itemId mockRecords
                let advice = generateAdvice trend

                -- 显示结果
                resultHtml <- UI.div #+ [
                    UI.h2 # set text ("物品ID: " ++ show itemId),
                    UI.p # set text ("当前价格: " ++ show (currentPrice trend)),
                    UI.p # set text ("平均价格: " ++ show (averagePrice trend)),
                    UI.p # set text ("价格变化: " ++ show (priceChange trend)),
                    UI.p # set text ("趋势: " ++ show (trend trend)),
                    UI.h3 # set text "交易建议",
                    UI.p # set text (advice advice),
                    UI.p # set text ("置信度: " ++ show (confidence advice)),
                    UI.p # set text ("风险等级: " ++ show (riskLevel advice))
                ]
                element resultArea # set children [resultHtml]
            _ -> element resultArea # set children [UI.p # set text "请输入有效的物品ID"]

    -- 设置样式
    getBody window # set style [
        ("font-family", "Arial, sans-serif"),
        ("max-width", "800px"),
        ("margin", "0 auto"),
        ("padding", "20px")
    ]
    element title # set style [
        ("color", "#333"),
        ("text-align", "center")
    ]
    element searchBox # set style [
        ("width", "70%"),
        ("padding", "10px"),
        ("margin-right", "10px")
    ]
    element searchButton # set style [
        ("padding", "10px 20px"),
        ("background-color", "#4CAF50"),
        ("color", "white"),
        ("border", "none"),
        ("border-radius", "4px")
    ]
    element resultArea # set style [
        ("margin-top", "20px"),
        ("padding", "20px"),
        ("border", "1px solid #ddd"),
        ("border-radius", "4px")
    ] 