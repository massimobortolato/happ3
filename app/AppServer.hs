module AppServer where

--------------------------------------------------------------------------------

import Coordinates (
  Coordinates (LatLonCoordinates),
  convertCoordinates,
 )

import Database
import Lucid (renderText)
import Network.Wai (Request (rawPathInfo, remoteHost))
import Network.Wai.Middleware.Static
import Pages
import Types
import Web.Scotty.Trans
import Prelude hiding (for_, get)

--------------------------------------------------------------------------------
type ScottyM = ScottyT AppM

--------------------------------------------------------------------------------
pullApi :: ScottyM ()
pullApi =
  get "/pull" do
    lift pullDB >>= json

--------------------------------------------------------------------------------
initApi :: ScottyM ()
initApi =
  get "/init" do
    lift initDB >> redirect "/"

--------------------------------------------------------------------------------
clearApi :: ScottyM ()
clearApi =
  get "/clear" do
    lift clearDB >> redirect "/"

--------------------------------------------------------------------------------
toUtmApi :: ScottyM ()
toUtmApi =
  get "/toUTM2/:latitude/:longitude" do
    (h :: Text) <- fmap (show . remoteHost) request
    (p :: Text) <- fmap (decodeUtf8 . rawPathInfo) request
    lift $ pushDB h p
    (lat :: Double) <- pathParam "latitude"
    (lon :: Double) <- pathParam "longitude"
    json $ convertCoordinates $ LatLonCoordinates lat lon

--------------------------------------------------------------------------------
rootApi :: ScottyM ()
rootApi =
  get "" do
    (lat :: Maybe Double) <- queryParamMaybe "latitude"
    (lon :: Maybe Double) <- queryParamMaybe "longitude"
    html $ renderText $ pageRoot $ doJob lat lon
 where
  doJob :: Maybe Double -> Maybe Double -> Maybe Coordinates
  doJob mlat mlon =
    convertCoordinates <$> (LatLonCoordinates <$> mlat <*> mlon)

--------------------------------------------------------------------------------
apis :: ScottyM ()
apis =
  toUtmApi
    >> initApi
    >> clearApi
    >> pullApi
    >> rootApi

middlewares :: ScottyM ()
middlewares =
  middleware $ staticPolicy $ addBase "static"

--------------------------------------------------------------------------------
mainScotty :: Int -> IO ()
mainScotty port = do
  conf <- newAppConfig
  scottyT port (morph conf) (middlewares <> apis)
 where
  morph = flip (runReaderT . runAppM)
  newAppConfig :: IO AppConfig
  newAppConfig =
    newDbPool <&> AppConfig
