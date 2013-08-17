module GoogleRequest where

import Data.List
import Data.Ratio

import Network.HTTP
import Network.Stream
import Network.URI

import Text.JSON as JSON
import Text.JSON.Pretty


{--
main = do
    url <- getLine
    process ["IG11 9HQ"] ["SE16 3EF"]
--}

process :: [String] -> [String] -> IO ()
process origins destinations = do
        let escapedUrl = requestString origins destinations
        response <- simpleHTTP (getRequest escapedUrl)
        let Right output = parseResponse response
        print $ sortDistanceMatrix output
        return ()         

urlDistanceMatrix = "http://maps.googleapis.com/maps/api/distancematrix/json?"

requestString :: [String] -> [String] -> String
requestString origins destinations = escapedUrl
    where
        escapedUrl = escapeURIString isUnescapedInURI (urlDistanceMatrix ++ resultString)
        resultString = intercalate "&" [originsString, destinationsString, sensorString]
        sensorString = "sensor=false"
        originsString = "origins=" ++ intercalate "|" origins
        destinationsString = "destinations=" ++ intercalate "|" destinations

parseResponse :: (Network.Stream.Result (Response String)) -> Either String [(String, [(String, Integer)])] 
parseResponse (Left _) = Left "Some error"
parseResponse (Right response) = Right out
    where
        out = parseAll . JSON.decode $ rspBody response

parseAll :: JSON.Result (JSObject JSValue) -> [(String, [(String, Integer)])] 
parseAll (Ok resp) = final
    where 
        objMap = fromJSObject resp
        Just (JSArray destinationList) = lookup "destination_addresses" objMap
        Just (JSArray originList) = lookup "origin_addresses" objMap
        destinations = map fromJSString [loc | JSString loc <- destinationList]
        origins = map fromJSString [loc | JSString loc <- originList]
        Just (JSArray rows) = lookup "rows" objMap
        rowObjects = map fromJSObject [row | JSObject row <- rows]        
        final = zip destinations . transpose $ pairOrigins origins (map parseRow rowObjects)

pairOrigins :: (Num a) => [String] -> [[a]] -> [[(String, a)]]
pairOrigins [] [] = []
pairOrigins (o:os) (m:ms) = [(o, m') | m' <- m]:(pairOrigins os ms)

--parseRow :: [(String, JSValue)] -> String
parseRow [("elements", JSArray elements)] = distanceOnly
    where
        elems = map fromJSObject [e | JSObject e <- elements]      
        distanceObjects = map (lookup "distance") elems
        realData = map fromJSObject [o | Just (JSObject o) <- distanceObjects] 
        distanceOnly = [numerator x | Just (JSRational _ x) <- map (lookup "value") realData]

-- TODO Rank our distance vectors
sortDistanceMatrix :: (Num a) => [(String, [(String, a)])] -> [(String, [(String, a)])]
sortDistanceMatrix list = list
