module Species (
    Species(..),
    speciesDistance, produceOffsprings, newSpecies, surviveSpecies
) where

-- import Debug.Trace
import Control.Monad.Random
import Data.List (sortBy, tails)
import Data.Ord (comparing)
import Genome

data Species = Species {
                   spId  :: Int,     -- identifier of the species
                   spGen :: Int,     -- generations of the species so far
                   spRep :: Genome,  -- genome representing the species
                   spPop :: [Genome] -- population
             }

speciesDistance :: EnvParams -> Genome -> Species -> Float
speciesDistance env g sp = genomeDistance env g (spRep sp)

{--
adjustedFitness :: Species -> [Float] -> [Float]
adjustedFitness sp = map (* a)
    where a = 1 / fromIntegral (length $ spPop sp)
--}

-- We reproduce a species like this:
-- - based on the sum of the adjusted fitness of a species we assign a number of offsprings
--   of that species for the next generation, k
-- - we want to generate at least 25% offsprings through mutation, the rest through cross over
-- - we want to generate offsprings by cross over of n best individuals, which would give
--   a total of n * (n-1) / 2 offsprings, where n = floor(0.5 * (1 + sqrt(1 + 8*k)))
--   (this should be maximum k offsprings)
-- - if we must generate more offsprings (k1 = k - n * (n-1) / 2), then we mutate the best k1
--   individuals of the species
-- The best individual remains as a representant of the new generation of the species
-- The offsprings have to be assigned in the next generation to the first species which is
-- compatible with it

produceOffsprings :: (RandomGen g, Monad m) => EnvParams -> MutateState -> (Int, [(Genome, Float)])
                                            -> RandT g m MutateState
produceOffsprings env (inno, innos, gs) (k, gfs) = do
    let gfss = reverse $ sortBy (comparing snd) gfs
        k' = fromIntegral k * 0.75 :: Double -- make parameter!
        n  = floor $ 0.5 * (1 + sqrt (1 + 8 * k'))
        -- cs = trace ("k = " ++ show k ++ ", n = " ++ show n) $ take n gfss -- these ones will be crossed over
        cs = take n gfss -- these ones will be crossed over
        nr = length cs -- nr should be at least 2, otherwise no cross over
        k1 = k - nr * (nr-1) `div` 2
        ms = take k1 $ cycle $ map fst gfss -- these ones will be mutated (as often as necessary)
        cps | nr >= 2   = concatMap (\(a, bs) -> zip (repeat a) bs) $ zip (init cs) (tail $ tails cs)
            | otherwise = []
    -- gcs <- if null cps then return [] else mapM (uncurry (crossOver env)) cps
    gcs <- mapM (uncurry (crossOver env)) cps
    (i, is, ngs) <- foldM (mutateGeneric env) (inno, innos, gs) ms
    return (i, is, gcs ++ ngs)

-- Initialize a new species with the given Id and population
newSpecies :: Int -> Genome -> [Genome] -> Species
newSpecies i g gs = Species { spId = i, spGen = 1, spRep = g, spPop = gs }

-- Empty species die
-- Also choose new representant only for older species
surviveSpecies :: [Species] -> [Species]
surviveSpecies = go
    where go [] = []
          go (s:ss) | null (spPop s) = go ss
                    | otherwise      = s1 : go ss
                    where s1 | spGen s == 1 = s
                             | otherwise    = s { spRep = head (spPop s) }
