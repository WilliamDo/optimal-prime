import Test.Hspec

import Data.Matrix as M
import DistanceMatrixRank

main :: IO ()
main = hspec $ do
  describe "rankDestinations" $ do
    it "simpleMatrix" $ do
      rankDestinations exampleMatrix `shouldBe` [("C", 0.0), ("D", 0.0)]

exampleMatrix = DistanceMatrix {distanceMatrix = M.fromLists [[1, 1], [1, 1]], origins = ["A", "B"], destinations = ["C", "D"]}
