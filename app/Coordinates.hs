{-# LANGUAGE DeriveAnyClass #-}

module Coordinates where

import Data.Aeson (ToJSON)

data Coordinates
  = Coordinates
      { latitude :: Double
      , longitude :: Double
      , latitudeBand :: Char
      , zone :: Int
      , easting :: Double
      , northing :: Double
      }
  | LatLonCoordinates
      { latitude :: Double
      , longitude :: Double
      }
  | UTMCoordinates
      { zone :: Int
      , latitudeBand :: Char
      , easting :: Double
      , northing :: Double
      }
  | Unimplemented
  deriving (Show, Generic, ToJSON)

--------------------------------------------------------------------------------
(|^) :: Double -> Int -> Double
(|^) = (^)

--------------------------------------------------------------------------------
convertCoordinates :: Coordinates -> Coordinates
convertCoordinates LatLonCoordinates{latitude, longitude} =
  let
    utmZone = floor ((longitude + 180) / 6) + 1 :: Int
    centralMeridian = fromIntegral $ (utmZone - 1) * 6 - 180 + 3 :: Double
    scaleFactor = 0.9996 :: Double
    equatorialRadius = 6378137 :: Double
    eccentricitySquared = 0.00669438 :: Double
    toRadians = (*) (pi / 180)
    lat = toRadians latitude
    n = equatorialRadius / sqrt (1 - eccentricitySquared * sin lat |^ 2)
    t = tan lat |^ 2
    c = eccentricitySquared * cos lat |^ 2
    a = cos lat * toRadians (longitude - centralMeridian)
    m =
      let
        r = equatorialRadius
        e = eccentricitySquared
       in
        r * ((1 - e / 4 - 3 * e |^ 2 / 64 - 5 * e |^ 3 / 256) * lat - (3 * e / 8 + 3 * e |^ 2 / 32 + 45 * e |^ 3 / 1024) * sin (2 * lat) + (15 * e |^ 2 / 256 + 45 * e |^ 3 / 1024) * sin (4 * lat) - (35 * e |^ 3 / 3072) * sin (6 * lat))
    easting = scaleFactor * n * (a + (1 - t + c) * a |^ 3 / 6 + (5 - 18 * t + t |^ 2 + 72 * c - 58 * eccentricitySquared) * a |^ 5 / 120) + 500000
    utmFalseNorthing = if latitude < 0 then 10000000 else 0 :: Double
    northing = scaleFactor * (m + n * tan lat * (a |^ 2 / 2 + (5 - t + 9 * c + 4 * c |^ 2) * a |^ 4 / 24 + (61 - 58 * t + t |^ 2 + 600 * c - 330 * eccentricitySquared) * a |^ 6 / 720)) + utmFalseNorthing
   in
    Coordinates
      { latitude = latitude
      , longitude = longitude
      , latitudeBand = latitudeBand' latitude
      , zone = utmZone
      , easting = easting
      , northing = northing
      }
convertCoordinates _ = Unimplemented

--------------------------------------------------------------------------------
latitudeBand' :: Double -> Char
latitudeBand' lat
  | lat >= 0 = go lat "NPQRSTUVWX"
  | otherwise = go (-lat) "MLKJHGFEDC"
 where
  go :: Double -> [Char] -> Char
  go n pat =
    case pat !!? floor (n / 8) of
      (Just c) -> c
      _ -> ' '