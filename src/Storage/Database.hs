{-# LANGUAGE OverloadedStrings #-}

module Storage.Database where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import Model.Item
import Data.Time
import Control.Monad.IO.Class

data DBConfig = DBConfig {
    dbPath :: String
}

newtype Database m = Database {
    connection :: Connection
}

instance FromRow PriceRecord where
    fromRow = PriceRecord <$> field <*> field <*> field <*> field <*> field

instance ToRow PriceRecord where
    toRow (PriceRecord i p t s q) = toRow (i, p, t, s, q)

initializeDB :: FilePath -> IO Connection
initializeDB dbPath = do
    conn <- open dbPath
    execute_ conn "CREATE TABLE IF NOT EXISTS price_records (\
        \item_id INTEGER,\
        \price REAL,\
        \timestamp TEXT,\
        \server TEXT,\
        \quantity INTEGER\
        \)"
    return conn

class Monad m => Storage m where
    savePriceRecord :: PriceRecord -> m ()
    getPriceRecords :: Int -> String -> m [PriceRecord]
    cleanupOldRecords :: NominalDiffTime -> m ()

instance MonadIO m => Storage (Database m) where
    savePriceRecord record = do
        liftIO $ execute (connection (Database undefined)) 
            "INSERT INTO price_records (item_id, price, timestamp, server, quantity) VALUES (?, ?, ?, ?, ?)"
            record

    getPriceRecords itemId server = do
        liftIO $ query (connection (Database undefined))
            "SELECT * FROM price_records WHERE item_id = ? AND server = ? ORDER BY timestamp DESC"
            (itemId, server)

    cleanupOldRecords days = do
        now <- liftIO getCurrentTime
        let cutoff = addUTCTime (-days * 24 * 60 * 60) now
        liftIO $ execute (connection (Database undefined))
            "DELETE FROM price_records WHERE timestamp < ?"
            (Only cutoff)

-- 辅助函数
getRecentPriceRecords :: MonadIO m => Database m -> Int -> String -> Int -> m [PriceRecord]
getRecentPriceRecords db itemId server limit = do
    liftIO $ query (connection db)
        "SELECT * FROM price_records WHERE item_id = ? AND server = ? ORDER BY timestamp DESC LIMIT ?"
        (itemId, server, limit)

getPriceHistory :: MonadIO m => Database m -> Int -> String -> Day -> Day -> m [PriceRecord]
getPriceHistory db itemId server startDate endDate = do
    liftIO $ query (connection db)
        "SELECT * FROM price_records WHERE item_id = ? AND server = ? AND date(timestamp) BETWEEN ? AND ? ORDER BY timestamp"
        (itemId, server, startDate, endDate) 