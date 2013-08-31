module GoogleRequest where

import Data.List
import Data.Ratio

import Network.HTTP
import Network.Stream
import Network.URI

import Text.JSON as JSON
import Text.JSON.Pretty

process :: [String] -> [String] -> IO ()
process origins destinations = do
        let escapedUrl = requestUrl origins destinations
        httpResponse <- simpleHTTP (getRequest escapedUrl)
        -- TODO Check if response came back okay before parsing
        responseBody <- getResponseBody httpResponse
        let (origins, destinations, matrix) = parseResponse responseBody
            (ranks, destMatrix) = rankDistanceMatrix matrix
            sorted = sort $ applyLabels destMatrix origins destinations ranks 
        print sorted
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

parseResponse :: String -> ([String], [String], [[Integer]]) 
parseResponse response = parseJSONResponse . decodeStrict $ response

parseJSONResponse :: JSON.Result (JSObject JSValue) -> ([String], [String], [[Integer]]) 
parseJSONResponse (Ok resp) = (origins, destinations, matrix)
    where 
        destinations = parseLocations "destination_addresses" resp
        origins = parseLocations "origin_addresses" resp
        matrix = parseMatrix resp

parseLocations :: String -> JSObject JSValue -> [String]
parseLocations locationType obj = map fromJSString [ l | JSString l <- dataList ]
    where
        objMap = fromJSObject obj
        Just (JSArray dataList) = lookup locationType objMap

parseMatrix :: JSObject JSValue -> [[Integer]]
parseMatrix obj = matrix
    where
        Just (JSArray rows) = lookup "rows" (fromJSObject obj)
        rowObjects = map fromJSObject [row | JSObject row <- rows]        
        matrix = map parseRow rowObjects

--parseRow :: [(String, JSValue)] -> String
parseRow [("elements", JSArray elements)] = distanceOnly
    where
        elems = map fromJSObject [e | JSObject e <- elements]      
        distanceObjects = map (lookup "distance") elems
        realData = map fromJSObject [o | Just (JSObject o) <- distanceObjects] 
        distanceOnly = [numerator x | Just (JSRational _ x) <- map (lookup "value") realData]

-- Rank our distance vectors
-- Current strategy uses standard deviation
rankDistanceMatrix :: (Integral a, Floating b) => [[a]] -> ([b], [[a]])
rankDistanceMatrix list = (ranks, byDestination)
    where
        ranks = map standardDeviation byDestination
        byDestination = transpose list

standardDeviation :: (Integral a, Floating b) => [a] -> b
standardDeviation items = stddev
    where
        mean = (fromIntegral $ sum items) / (fromIntegral $ length items)
        diffs = [ abs((fromIntegral x) - mean) | x <- items ]
        stddev = sqrt ((sum $ map (^ 2) diffs  ) / (fromIntegral $ length items))   

-- Rough
applyLabels :: Floating a => [[Integer]] -> [String] -> [String] -> [a] -> [(a, String, [(String, Integer)])]
applyLabels matrix origins destinations ranks = zip3 ranks destinations pairOrigins
    where
        pairOrigins = map (zip origins) matrix
 
