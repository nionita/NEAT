-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.
import Control.Monad.Random
import Genome
import Species
import Evolution

fitXOR :: Fit IO
fitXOR net = do
  let ins = [[0, 0], [0, 1], [1, 0], [1, 1]]
      tru = [    0,      1,      1,      0]
      est = map net ins
      -- los = 4 - (sum $ map (err . abs) $ zipWith subtract tru $ map head est)
      los = 4 - (sum $ map abs $ zipWith subtract tru $ map head est)
      lol = los * los
  return lol
  where err x | x < 0.4   = 0
              | otherwise = 1

hookXOR :: Hook IO
hookXOR gs st best = do
  when (esGen st `mod` 25 == 0 || best) $ do
      putStrLn ""
      putStrLn $ "Generation " ++ show (esGen st)
      putStrLn "----------------"
      -- putStrLn "Generated offsprings"
      -- mapM_ showGenome gs
      case esBestGenome st of
          Nothing -> putStrLn "No best genome yet"
          Just g  -> do
              putStrLn $ "Best genome so far with fitness " ++ show (esBestFitness st) ++ ":"
              showGenome g
  when (esGen st `mod` 25 == 0) $ do
      putStrLn $ "Total species: " ++ show (length $ esSpecies st)
      mapM_ showSpecies $ esSpecies st
  if esGen st > 100 then return False else return True

env :: EnvParams
env = EnvParams {
          envTotPopulation = 150,
          envInNodes = 2, envOutNodes = 1,
          envC1 = 2, envC2 = 2, envC3 = 0.4,
          envDistThres = 3,
          envProbNewWeight = 0.2,
          envRatioAddConn = 10, envRatioAddNode = 5, envRatioWeights = 90
    }

main :: IO ()
main = evalRandTIO $ doEvolution env fitXOR hookXOR

showGenome :: Genome -> IO ()
showGenome genome = do
  putStrLn $ "- hiddenNodes = " ++ show (hiddenNodes genome) ++ ", origin = " ++ show (origin genome)
  forM_ (genes genome) $ \g -> putStrLn ("- " ++ show g)

showSpecies :: Species -> IO ()
showSpecies s = do
  putStrLn $ "Species " ++ show (spId s) ++ ", generation " ++ show (spGen s)
  putStrLn "Represented by:"
  showGenome $ spRep s
  putStrLn "Population is:"
  mapM_ showGenome $ spPop s
