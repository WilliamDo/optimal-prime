{-# LANGUAGE OverloadedStrings #-}

module GoogleRequest where

import Data.Aeson
import Data.Aeson.Types
import Data.List
import Data.Matrix
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as S
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as C

import Network.HTTP
import Network.Stream
import Network.URI

import Control.Applicative

import qualified DistanceMatrixRank as DMR

data GoogleMatrix = GoogleMatrix
  {
    origins :: [String],
    destinations :: [String],
    rows :: [Row]
  }
  deriving (Show)

data ElementEntry = ElementEntry
  {
    text :: String,
    value :: Double
  }
  deriving (Show)

data Element = Element
  {
    distance :: ElementEntry,
    duration :: ElementEntry
  }
  deriving (Show)

data Row = Row [Element]
  deriving (Show)

instance FromJSON GoogleMatrix where
  parseJSON (Object v) = GoogleMatrix <$>
                          v .: "origin_addresses" <*>
                          v .: "destination_addresses" <*>
                          v .: "rows"

instance FromJSON ElementEntry where
  parseJSON (Object v) = ElementEntry <$> v.: "text" <*> v.: "value"

instance FromJSON Element where
  parseJSON (Object v) = Element <$> v.: "distance" <*> v.: "duration"

instance FromJSON Row where
  parseJSON (Object v) = Row <$> v.: "elements"

process :: [String] -> [String] -> IO ()
process from to = do
        let escapedUrl = requestUrl from to
        httpResponse <- simpleHTTP (getRequest escapedUrl) >>= getResponseBody
        let (Just x) = decode (C.pack httpResponse) :: Maybe GoogleMatrix
        let m = DMR.rankDestinations $ convert x
        putStrLn (show m)
        return ()

urlDistanceMatrix = "http://maps.googleapis.com/maps/api/distancematrix/json?"

requestUrl :: [String] -> [String] -> String
requestUrl origins destinations = escapedUrl
    where
        escapedUrl = escapeURIString isUnescapedInURI (urlDistanceMatrix ++ resultString)
        resultString = intercalate "&" [originsString, destinationsString, sensorString]
        sensorString = "sensor=false"
        originsString = "origins=" ++ intercalate "|" origins
        destinationsString = "destinations=" ++ intercalate "|" destinations

rowDistances :: Row -> [Double]
rowDistances (Row elements) = distanceVals
  where
    distanceOnly = map distance elements
    distanceVals = [ value d | d <- distanceOnly]

convert :: GoogleMatrix -> DMR.DistanceMatrix
convert (GoogleMatrix os ds rs) = DMR.DistanceMatrix m os ds
  where
    m = fromLists $ map rowDistances rs
