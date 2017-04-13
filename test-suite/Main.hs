-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
import qualified Test.Tasty
-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.
import Test.Tasty.Hspec

import Data.IntSet (elems)
import Genome

main :: IO ()
main = do
    test <- testSpec "NEAT" spec
    Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $ do
    let connections = map P [(1, 5), (2, 5), (3, 4), (3, 5)]
    it "finds existent connections" $ do
        let a = (3, 4) :: (Int, Int)
            b = (1, 5) :: (Int, Int)
        (a `connectedIn` connections) `shouldBe` True
        (b `connectedIn` connections) `shouldBe` True
    it "detects non existent connections" $ do
        let b = (5, 1) :: (Int, Int)
            a = (4, 7) :: (Int, Int)
        (b `connectedIn` connections) `shouldBe` False
        (a `connectedIn` connections) `shouldBe` False
    it "computes the closure correctly" $ do
        let a = 3 :: Int
            b = 1 :: Int
        elems (closure connections a) `shouldBe` [3, 4, 5]
        elems (closure connections b) `shouldBe` [1, 5]

newtype P = P (Int, Int)

instance Connection P where
    fromNode (P (a, _)) = a
    toNode   (P (_, b)) = b
