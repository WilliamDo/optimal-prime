module DistanceMatrixRank where

import Data.List as L
import Data.Vector as V
import Data.Matrix as M

import Statistics.Sample

data DistanceMatrix = DistanceMatrix 
									{ 
										distanceMatrix :: Matrix Double, 
										origins :: [String], 
										destinations :: [String] 
									}
									deriving (Show)

rankDestinations :: DistanceMatrix -> [(String, Double)]
rankDestinations m = L.zip (destinations m) (L.map stdDev $ columns (distanceMatrix m))

columns :: Matrix a -> [Vector a]
columns m = L.map (flip M.getCol m) [1..cols]
	where
		cols = M.ncols m

testMatrix = DistanceMatrix {distanceMatrix = M.fromLists [[1, 2], [3, 100]], origins = ["A", "B"], destinations = ["C", "D"]}

