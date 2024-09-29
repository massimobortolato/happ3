{-# LANGUAGE DeriveAnyClass #-}

module Database (
  initDB,
  pushDB,
  clearDB,
  pullDB,
  newDbPool,
) where

--------------------------------------------------------------------------------

import Control.Exception (catch)
import Data.Aeson
import Data.Pool
import Data.Time
import Database.PostgreSQL.Simple
import Types

--------------------------------------------------------------------------------
data User = User
  { from :: Text
  , path :: Text
  , time :: UTCTime
  }
  deriving (Show, Generic, ToRow, ToJSON, FromRow)

--------------------------------------------------------------------------------
initDB :: AppM ()
initDB = do
  pool <- asks dbPool
  liftIO $ withResource pool go
 where
  go mconn = safeExecute_ mconn "create table if not exists users (fromip varchar(64), path varchar(64), time timestamp with time zone)"

--------------------------------------------------------------------------------
pushDB :: Text -> Text -> AppM ()
pushDB ip path = do
  pool <- asks dbPool
  ur <- liftIO $ getCurrentTime <&> User ip path
  liftIO $ withResource pool (go ur)
 where
  go ur mconn = safeExecute mconn "insert into users (fromip, path, time) values (?,?,?)" ur

--------------------------------------------------------------------------------
pullDB :: AppM [User]
pullDB = do
  pool <- asks dbPool
  liftIO $ withResource pool go
 where
  go mconn = safeQuery_ mconn "select * from users order by time desc limit 20"

--------------------------------------------------------------------------------
clearDB :: AppM ()
clearDB = do
  pool <- asks dbPool
  liftIO $ withResource pool go
 where
  go mconn = safeExecute_ mconn "delete from users"

--------------------------------------------------------------------------------
connectInfo :: ConnectInfo
connectInfo =
  ConnectInfo
    "localhost"
    5432
    "postgres"
    "pippo"
    "postgres"

--------------------------------------------------------------------------------
newDbPool :: IO (Pool MaybeConnection)
newDbPool =
  newPool $
    defaultPoolConfig
      create
      destroy
      sleepTime
      numOfConnections
 where
  create = do
    putStrLn "Connect to DB..."
    catch (connect connectInfo <&> Just) \(SomeException e) -> do
      putStrLn $ "Error while opening connection to DB\n" <> show e
      pure Nothing

  destroy mconn
    | (Just c) <- mconn = do
        putStrLn "Close"
        close c
    | otherwise =
        pure ()

  sleepTime = 10 :: Double
  numOfConnections = 10

--------------------------------------------------------------------------------
safely :: MaybeConnection -> (Connection -> IO r) -> r -> IO r
safely mconn f def
  | (Just conn) <- mconn =
      f conn `catch` handler
  | otherwise = do
      putStrLn "No DB connection"
      pure def
 where
  handler (SomeException e) = do
    putStrLn $ "Error:" <> show e
    pure def

--------------------------------------------------------------------------------
safeQuery_ :: forall r. (FromRow r) => MaybeConnection -> Query -> IO [r]
safeQuery_ mconn q =
  safely mconn (`query_` q) []

--------------------------------------------------------------------------------
safeExecute :: forall q. (ToRow q) => MaybeConnection -> Query -> q -> IO ()
safeExecute mconn quer q =
  safely mconn (flip3 execute quer q) ()
 where
  flip3 f a2 a3 a1 = void $ f a1 a2 a3

--------------------------------------------------------------------------------
safeExecute_ :: MaybeConnection -> Query -> IO ()
safeExecute_ mconn quer =
  safely mconn (flip2 execute_ quer) ()
 where
  flip2 f a2 a1 = void $ f a1 a2
