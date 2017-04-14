module Species (
    Species(..),
    adjustedFitness, speciesDistance, produceOffsprings
) where

import Control.Monad.Random
import Data.List (sortBy, tails)
import Data.Ord (comparing)
import Genome

data Species = Species {
                   spId  :: Int,    -- identifier of the species
                   spGen :: Int,    -- generations of the species so far
                   spRep :: Genome, -- genome representing the species
                   spPop :: [Genome], -- population
                   spFit :: Float   -- sum of the fitness of the species
             }

speciesDistance :: EnvParams -> Species -> Genome -> Float
speciesDistance env sp = genomeDistance env (spRep sp)

adjustedFitness :: Species -> [Float] -> [Float]
adjustedFitness sp = map (* a)
    where a = 1 / fromIntegral (length $ spPop sp)

-- We reproduce a species like this:
-- - based on the sum of the adjusted fitness of a species we assign a number of offsprings
--   of that species for the next generation, k
-- - we want to generate offsprings by cross over of n best individuals, which would give
--   a total of n * (n-1) / 2 offsprings, where n = floor(0.5 * (1 + sqrt(1 + 8*k)))
--   (this should be maximum k offsprings)
-- - if we must generate more offsprings (k1 = k - n * (n-1) / 2), then we mutate the best k1
--   individuals of the species
-- The best individual remains as a representant of the new generation of the species
-- The offsprings have to be assigned in the next generation to the first species which is
-- compatible with it

produceOffsprings :: (RandomGen g, Monad m) => EnvParams -> Int -> MutateState
                                            -> [(Genome, Float)] -> RandT g m MutateState
produceOffsprings env k (inno, innos, gs) gfs = do
    let gfss = reverse $ sortBy (comparing snd) gfs
        n  = floor $ 0.5 * ((1::Double) + sqrt (1 + 8 * fromIntegral k))
        cs = take n gfss -- these ones will be crossed over
        nr = length cs
        k1 = k - nr * (nr-1) `div` 2
        ms = take k1 $ cycle $ map fst gfss -- these ones will be mutated (as often as necessary)
        cps = concatMap (\(a, bs) -> zip (repeat a) bs) $ zip (init cs) (tail $ tails cs)
    gcs <- mapM (uncurry (crossOver env)) cps
    (i, is, ngs) <- foldM (mutateGeneric env) (inno, innos, gs) ms
    return (i, is, gcs ++ ngs)
