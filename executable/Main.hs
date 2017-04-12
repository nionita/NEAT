-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.
import Genome
import Network

main :: IO ()
main = do
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
