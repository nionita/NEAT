module Evolution (
    EvState(..), Fit, Hook,
    doEvolution
) where

import Control.Monad.Random
import Data.List (maximumBy)
import Data.Ord (comparing)
import Genome
import Species
import Network

data EvState = EvState {
                   esInno, esGen, esSpec :: Int, -- next innovation number, generations, next species
                   esSpecies :: [Species],       -- list of species
                   esBestGenome :: Maybe Genome, -- best genome so far
                   esBestFitness :: Float        -- best fitness so far
             }

type Fit m = Network -> m Float
type Hook m = [Genome] -> EvState -> Bool -> m Bool

-- This is the main function, which starts an evolution given parameters
-- and 2 monadic functions
doEvolution :: CtxRandom g m => EnvParams -> Fit m -> Hook m -> RandT g m ()
doEvolution env fit hook = initState env hook >>= go
    where go s = do
              ms1 <- evoStep env fit hook s
              case ms1 of
                  Just s1 -> go s1
                  Nothing -> return ()

-- Initialize the population with total number of genomes, all having the correct
-- input & output nodes numbers and 0 hidden nodes, and exctly 1 (random) connection
initializePopulation :: CtxRandom g m => EnvParams -> RandT g m (Int, [Genome])
initializePopulation env = go [] 1 [] (envTotPopulation env)
    where genome0 = Genome { hiddenNodes = 0, origin = 0, genes = [] }
          go acc inno _     0 = return (inno, acc)
          go acc inno innos r = do
              mr <- mutateAddConnection env inno innos genome0
              case mr of
                  Nothing         -> go acc     inno innos r
                  Just (i, is, g) -> go (g:acc) i    is    (r-1)

initState :: CtxRandom g m => EnvParams -> Hook m -> RandT g m EvState
initState env hook = do
    (inno, gs) <- initializePopulation env
    let sp0 = newSpecies 0 (head gs) gs
        st = EvState {
                 esInno = inno, esGen = 1, esSpec = 1, esSpecies = [sp0],
                 esBestGenome = Nothing, esBestFitness = 0
             }
    _ <- lift $ hook [] st False -- in init we ignore the hook result
    return st

-- Execute one evolution step
-- Total population may vary because of rounding
-- Monadic functions to compute the fitness and evaluate the state are given as parameters
evoStep :: CtxRandom g m => EnvParams -> Fit m -> Hook m -> EvState -> RandT g m (Maybe EvState)
evoStep env fit hook st = do
    -- calculate fitness for every genome in every species
    fitss <- mapM (lift . fitSpecies env fit) (esSpecies st)
    let sumfits = map sum fitss -- sums of fitness per species
        sumfit  = sum sumfits   -- sum of all fitness
        nospcs  = length (esSpecies st) -- number of species
        avg     = fromIntegral (envTotPopulation env) / fromIntegral nospcs :: Double
        invsf   = fromIntegral (envTotPopulation env) / sumfit
        -- number of offsprings in next generation per species
        ofspng | sumfit == 0 = take nospcs $ repeat $ round avg
               | otherwise   = map round $ map (* invsf) sumfits
        spcfits = map (\(s, fs) -> zip (spPop s) fs) $ zip (esSpecies st) fitss
        -- best in this generation & until now
        (bg, bf) = maximumBy (comparing snd) $ concat spcfits
        (bgs, bfs) = case esBestGenome st of
                         Nothing -> (Just bg, bf)
                         Just _  -> if bf > esBestFitness st
                                       then (Just bg, bf)
                                       else (esBestGenome st, esBestFitness st)
    -- produce the offsprings
    (i1, _, gs) <- foldM (produceOffsprings env) (esInno st, [], []) $ zip ofspng spcfits
    let nspcs1 = map (\s -> s { spPop = [], spGen = spGen s + 1 }) (esSpecies st)
        (sid, nspcs2) = foldr (insertOffspring env) (esSpec st, nspcs1) gs
        nspcs3 = surviveSpecies nspcs2
        nst = EvState { esInno = i1, esGen = esGen st + 1, esSpec = sid,
                        esSpecies = nspcs3, esBestGenome = bgs, esBestFitness = bfs }
    -- We call the hook, which could decide to stop the evolution by returning False
    r <- lift $ hook gs nst (bfs > esBestFitness st)
    if r then return (Just nst) else return Nothing

insertOffspring :: EnvParams -> Genome -> (Int, [Species]) -> (Int, [Species])
insertOffspring env g (i, spcs) = go [] spcs
    where go acc [] = (i+1, reverse $ newSpecies i g [g] : acc)
          go acc (s:ss)
              | speciesDistance env g s <= envDistThres env = (i, reverse $ (s1:acc) ++ ss)
              | otherwise                                   = go (s:acc) ss
              where s1 = s { spPop = g : spPop s }

fitSpecies :: Monad m => EnvParams -> (Network -> m Float) -> Species -> m [Float]
fitSpecies env fit = mapM fit . map (genomeToFunction env) . spPop
