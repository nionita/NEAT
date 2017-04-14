-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
import qualified Test.Tasty
-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.
import Test.Tasty.Hspec

import Control.Monad.Random
import Data.IntSet (elems)
import Data.List (sort)
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
    it "genome oprations preserve the gene order" $ do
        [inno1, inno2, inno3, inno4, inno5, inno6, inno7, inno8, inno9, inno10, inno11, inno12]
            <- someGenomeOperations
        inno1 `shouldBe` sort inno1
        inno2 `shouldBe` sort inno2
        inno3 `shouldBe` sort inno3
        inno4 `shouldBe` sort inno4
        inno5 `shouldBe` sort inno5
        inno6 `shouldBe` sort inno6
        inno7 `shouldBe` sort inno7
        inno8 `shouldBe` sort inno8
        inno9 `shouldBe` sort inno9
        inno10 `shouldBe` sort inno10
        inno11 `shouldBe` sort inno11
        inno12 `shouldBe` sort inno12

someGenomeOperations :: IO [[Int]]
someGenomeOperations =
    evalRandTIO $ do
         let env = EnvParams { inNodes = 3, outNodes = 1 }
             genome0 = Genome { hiddenNodes = 0, genes = [] }
             i0  = 1
             is0 = []
         mr1 <- mutateAddConnection env i0 is0 genome0
         let (i1, is1, genome1) = case mr1 of
                 Just r  -> r
                 Nothing -> (i0, is0, genome0)
         mr2 <- mutateAddConnection env i1 is1 genome1
         let (i2, is2, genome2) = case mr2 of
                 Just r  -> r
                 Nothing -> (i1, is1, genome1)
         mr3 <- mutateAddConnection env i2 is2 genome2
         let (i3, is3, genome3) = case mr3 of
                 Just r  -> r
                 Nothing -> (i2, is2, genome2)
         (i4, is4, genome4) <- mutateAddNode env i3 is3 genome3
         (i5, is5, genome5) <- mutateAddNode env i4 is4 genome3
         (i6, is6, genome6) <- mutateAddNode env i5 is5 genome3
         genome7 <- mutateWeights 0.2 genome4
         genome8 <- mutateWeights 0.2 genome5
         genome9 <- mutateWeights 0.2 genome6
         genome10 <- crossOver env (genome7, 10) (genome8, 11)
         genome11 <- crossOver env (genome8, 11) (genome9, 12)
         genome12 <- crossOver env (genome9, 12) (genome7, 10)
         return [innovations genome1, innovations genome2, innovations genome2
                , innovations genome2, innovations genome2, innovations genome2
                , innovations genome2, innovations genome2, innovations genome2
                , innovations genome10, innovations genome11, innovations genome12]

innovations :: Genome -> [Int]
innovations = map innov . genes

newtype P = P (Int, Int)

instance Connection P where
    fromNode (P (a, _)) = a
    toNode   (P (_, b)) = b
