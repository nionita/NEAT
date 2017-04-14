-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.
import Control.Monad.Random
import Genome
import Network
import Species

main1 :: IO ()
main1 = do
  let genome = Genome {
                   hiddenNodes = 1,
                   genes = [gene1, gene2, gene3, gene4, gene5]
               }
      gene1 = Gene { inNode = 1, outNode = 4, innov = 1, weight =  0.1, enabled = True }
      gene2 = Gene { inNode = 1, outNode = 5, innov = 2, weight =  0.2, enabled = True }
      gene3 = Gene { inNode = 2, outNode = 5, innov = 3, weight = -0.3, enabled = True }
      gene4 = Gene { inNode = 5, outNode = 4, innov = 4, weight = -0.4, enabled = True }
      gene5 = Gene { inNode = 3, outNode = 4, innov = 5, weight =  0.5, enabled = True }
      myNet = genomeToFunction env genome
      input1 = [1, 2, 3]
      input2 = [3, 4, 5]
      out1 = myNet input1
      out2 = myNet input2
  putStrLn $ "Result for input 1: " ++ show out1
  putStrLn $ "Result for input 2: " ++ show out2

env :: EnvParams
env = EnvParams {
          inNodes = 3, outNodes = 1,
          c1 = 1, c2 = 1, c3 = 0.4,
          distThreshold = 3,
          probNewWeight = 0.2,
          ratioAddConn = 15, ratioAddNode = 10, ratioWeights = 90
    }

main :: IO ()
main = evalRandTIO $ do
  let genome0 = Genome { hiddenNodes = 0, genes = [] }
      totPop = 20
      pop0 = take totPop $ repeat genome0
      k = 100 * (ratioAddConn env + ratioAddNode env + ratioWeights env)
  (_, _, pop) <- produceOffsprings env { ratioAddConn = k} totPop (1, [], []) $ zip pop0 $ repeat 0
  (_, _, gs)  <- produceOffsprings env totPop (1, [], []) $ zip pop $ repeat 0
  liftIO $ forM_ (zip [(1::Int) ..] gs) $ \(i, g) -> do
    putStrLn $ "Genome " ++ show i
    showGenome g

showGenome :: Genome -> IO ()
showGenome genome = do
  putStrLn $ "- hiddenNodes = " ++ show (hiddenNodes genome)
  forM_ (genes genome) $ \g -> putStrLn ("- " ++ show g)
