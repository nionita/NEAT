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
main2 = do
  let genome0 = Genome {
                    inNodes = 3, outNodes = 1, hiddenNodes = 0, genes = []
                }
      i0  = 1
      is0 = []
  mr1 <- evalRandTIO $ mutateAddConnection i0 is0 genome0
  let (i1, is1, genome1) = case mr1 of
          Just r  -> r
          Nothing -> (i0, is0, genome0)
  mr2 <- evalRandTIO $ mutateAddConnection i1 is1 genome1
  let (i2, is2, genome2) = case mr2 of
          Just r  -> r
          Nothing -> (i1, is1, genome1)
  mr3 <- evalRandTIO $ mutateAddConnection i2 is2 genome2
  let (i3, is3, genome3) = case mr3 of
          Just r  -> r
          Nothing -> (i2, is2, genome2)
  putStrLn "addConnection 1:"
  showGenome genome1
  putStrLn "addConnection 2:"
  showGenome genome2
  putStrLn "addConnection 3:"
  showGenome genome3
  (i4, is4, genome4) <- evalRandTIO $ mutateAddNode i3 is3 genome3
  (i5, is5, genome5) <- evalRandTIO $ mutateAddNode i4 is4 genome4
  (i6, is6, genome6) <- evalRandTIO $ mutateAddNode i5 is5 genome5
  putStrLn "====================="
  putStrLn "addNode 1:"
  showGenome genome4
  putStrLn "addNode 2:"
  showGenome genome5
  putStrLn "addNode 3:"
  showGenome genome6
  genome7 <- evalRandTIO $ mutateWeights 0.2 genome6
  genome8 <- evalRandTIO $ mutateWeights 0.2 genome7
  putStrLn "====================="
  putStrLn "mutate weights:"
  showGenome genome7
  putStrLn "mutate weights:"
  showGenome genome8

showGenome :: Genome -> IO ()
showGenome genome = do
  putStrLn $ "- inNodes = " ++ show (inNodes genome) ++ ", outNodes = " ++ show (outNodes genome)
      ++ ", hiddenNodes = " ++ show (hiddenNodes genome)
  forM_ (genes genome) $ \g -> putStrLn ("- " ++ show g)
