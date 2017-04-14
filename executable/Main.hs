-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.
import Control.Monad.Random
import Genome
import Network

main :: IO ()
main = main2

main1 :: IO ()
main1 = do
  let genome = Genome {
                   inNodes = 3, outNodes = 1, hiddenNodes = 1,
                   genes = [gene1, gene2, gene3, gene4, gene5]
               }
      gene1 = Gene { inNode = 1, outNode = 4, innov = 1, weight =  0.1, enabled = True }
      gene2 = Gene { inNode = 1, outNode = 5, innov = 2, weight =  0.2, enabled = True }
      gene3 = Gene { inNode = 2, outNode = 5, innov = 3, weight = -0.3, enabled = True }
      gene4 = Gene { inNode = 5, outNode = 4, innov = 4, weight = -0.4, enabled = True }
      gene5 = Gene { inNode = 3, outNode = 4, innov = 5, weight =  0.5, enabled = True }
      myNet = genomeToFunction genome
      input1 = [1, 2, 3]
      input2 = [3, 4, 5]
      out1 = myNet input1
      out2 = myNet input2
  putStrLn $ "Result for input 1: " ++ show out1
  putStrLn $ "Result for input 2: " ++ show out2

main2 :: IO ()
main2 = evalRandTIO $ do
  let genome0 = Genome {
                    inNodes = 3, outNodes = 1, hiddenNodes = 0, genes = []
                }
      i0  = 1
      is0 = []
  mr1 <- mutateAddConnection i0 is0 genome0
  let (i1, is1, genome1) = case mr1 of
          Just r  -> r
          Nothing -> (i0, is0, genome0)
  mr2 <- mutateAddConnection i1 is1 genome1
  let (i2, is2, genome2) = case mr2 of
          Just r  -> r
          Nothing -> (i1, is1, genome1)
  mr3 <- mutateAddConnection i2 is2 genome2
  let (i3, is3, genome3) = case mr3 of
          Just r  -> r
          Nothing -> (i2, is2, genome2)
  liftIO $ do
    putStrLn "addConnection 0 -> 1:"
    showGenome genome1
    putStrLn "addConnection 1 -> 2:"
    showGenome genome2
    putStrLn "addConnection 2 -> 3:"
    showGenome genome3
    putStrLn "====================="
  (i4, is4, genome4) <- mutateAddNode i3 is3 genome3
  (i5, is5, genome5) <- mutateAddNode i4 is4 genome3
  (i6, is6, genome6) <- mutateAddNode i5 is5 genome3
  liftIO $ do
    putStrLn "addNode 3 -> 4:"
    showGenome genome4
    putStrLn "addNode 3 -> 5:"
    showGenome genome5
    putStrLn "addNode 3 -> 6:"
    showGenome genome6
    putStrLn "====================="
  genome7 <- mutateWeights 0.2 genome4
  genome8 <- mutateWeights 0.2 genome5
  genome9 <- mutateWeights 0.2 genome6
  liftIO $ do
    putStrLn "mutate weights 4 -> 7:"
    showGenome genome7
    putStrLn "mutate weights: 5 -> 8"
    showGenome genome8
    putStrLn "mutate weights: 6 -> 9"
    showGenome genome9
    putStrLn "====================="
  genome10 <- crossOver (genome7, 10) (genome8, 11)
  genome11 <- crossOver (genome8, 11) (genome9, 12)
  genome12 <- crossOver (genome9, 12) (genome7, 10)
  liftIO $ do
    putStrLn "cross over 7, 8 -> 10:"
    showGenome genome10
    putStrLn "cross over 8, 9 -> 11:"
    showGenome genome11
    putStrLn "cross over 9, 7 -> 12:"
    showGenome genome12

showGenome :: Genome -> IO ()
showGenome genome = do
  putStrLn $ "- inNodes = " ++ show (inNodes genome) ++ ", outNodes = " ++ show (outNodes genome)
      ++ ", hiddenNodes = " ++ show (hiddenNodes genome)
  forM_ (genes genome) $ \g -> putStrLn ("- " ++ show g)
