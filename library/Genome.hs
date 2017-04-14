module Genome (
    EnvParams(..), Gene(..), Genome(..), Connection(..), Innovations, MutateState,
    mutateAddConnection, mutateAddNode, mutateWeights, crossOver, mutateGeneric,
    genomeDistance,
    closure, connectedIn
) where

import Control.Monad.Random
import Data.IntSet (IntSet, member, union, unions, singleton)
import Data.List (partition)

{--
The environment in which the NEAT problem is solved fixes the number of inputs and
the number of outputs of a network. Also a special input, the bias, which is always value 1,
is always present (node 0).
The number of inputs and outputs are kept, together with other parameters, in the EnvParams
data structure.

The genotype is represented as:
- the number of hidden nodes (could be 0)
- a list of connection genes, describing the connections of the network and their weights

Connections go from one node to another with following restrictions:
- an input node cannot appear as a destination (same for the bias)
- connections cannot form cycles (this is a marcant difference to the original NEAT paper)

The nodes are numbered as follows:
- node 0: the bias input (has always value 1)
- node 1 to inNodes: the input nodes
- node inNodes + 1 to inNodes + outNodes: the output nodes
- nodes greater than inNodes + outNodes (if any): hidden nodes
--}

data EnvParams = EnvParams {
                     inNodes, outNodes :: Int, -- input & output nodes of the problem
                     c1, c2, c3 :: Float,      -- coefficients for species distance
                     distThreshold :: Float,   -- distance threshold for speciation
                     probNewWeight :: Float,   -- probability of new weight when mutating weights
                     ratioAddConn, ratioAddNode, ratioWeights :: Rational
               }

data Gene = Gene {
                inNode, outNode, innov :: Int,
                weight :: Float,
                enabled :: Bool
          } deriving Show

data Genome = Genome {
                  hiddenNodes :: Int,
                  genes :: [Gene]
            } deriving Show

type Innovations = [((Int, Int), Int)]
type MutateState = (Int, Innovations, [Genome])

weightRange :: Float
weightRange = 10  -- value range for new weights

mutateGeneric :: (RandomGen g, Monad m) => EnvParams -> MutateState -> Genome
                                        -> RandT g m MutateState
mutateGeneric env (inno, innos, gs) genome = do
    c <- fromList [(1::Int, ratioAddConn env), (2, ratioAddNode env), (3, ratioWeights env)]
    case c of
        1 -> do mr <- mutateAddConnection env inno innos genome
                case mr of
                    Just (i, is, g) -> return (i, is, g:gs)
                    Nothing         -> do  -- fall back on mutating weights
                        g <- mutateWeights env genome
                        return (inno, innos, g:gs)
        2 -> do (i, is, g) <- mutateAddNode env inno innos genome
                return (i, is, g:gs)
        _ -> do g <- mutateWeights env genome
                return (inno, innos, g:gs)

-- Reusing the innovation numbers facilitates the crossovers
reuseInnovation :: Int -> Int -> (Int, Innovations) -> (Int, (Int, Innovations))
reuseInnovation f t (inno, innos)
    = case lookup (f, t) innos of
          Nothing -> (inno, (inno + 1, ((f, t), inno) : innos)) -- consume
          Just iv -> (iv,   (inno, innos))                      -- reuse

-- We have to add connections in such a way that we do not create cycles
-- If we have chosen the wrong nodes, we may fail
-- Same innovation in the same generation will get the same number (reuse)
mutateAddConnection :: (RandomGen g, Monad m) => EnvParams -> Int -> Innovations -> Genome
                                              -> RandT g m (Maybe (Int, Innovations, Genome))
mutateAddConnection env inno innos genome = do
    let maf = inNodes env + outNodes env + hiddenNodes genome
        mit = inNodes env + 1
        gs  = filter enabled (genes genome)
    f <- getRandomR (0,   maf) -- every node can be source
    t <- getRandomR (mit, maf) -- bias and inputs can't be destination
    if f == t || (f, t) `connectedIn` (genes genome) || f `member` (closure gs t)
       then return Nothing
       else do
           w <- getRandomR (-weightRange, weightRange)
           let (innog, (inno1, innos1)) = reuseInnovation f t (inno, innos)
               gene = Gene { inNode = f, outNode = t, innov = innog, weight = w, enabled = True }
               genome2 = genome { genes = insertGene gene (genes genome) }
           return $ Just (inno1, innos1, genome2)

-- We want to keep the genes ordered by innovation, so we can crossover fast
insertGene :: Gene -> [Gene] -> [Gene]
insertGene = go []
    where go acc g [] = reverse (g:acc)
          go acc g gs@(g1:gs1)
              | innov g < innov g1 = reverse (g:acc) ++ gs
              | otherwise          = go (g1:acc) g gs1

-- Add node splits one connection in 2, with a new (hidden) node in between
-- Only enabled connections can be chosen
mutateAddNode :: (RandomGen g, Monad m) => EnvParams -> Int -> Innovations -> Genome
                                        -> RandT g m (Int, Innovations, Genome)
mutateAddNode env inno innos genome = do
    let cs = filter enabled (genes genome)
    if null cs
       then return (inno, innos, genome)
       else do
           co <- uniform cs
           let cd = co { enabled = False }
               h = hiddenNodes genome + 1
               n = inNodes env + outNodes env + h
               (innog1, (inno1, innos1)) = reuseInnovation (inNode co) n  (inno,  innos)
               (innog2, (inno2, innos2)) = reuseInnovation n (outNode co) (inno1, innos1)
               gene1 = co { outNode = n, innov = innog1, weight = 1 }
               gene2 = co { inNode = n,  innov = innog2 }
               gs = insertGene gene1 $ insertGene gene2 $ replaceGene (innov co) cd (genes genome)
               genome2 = genome { hiddenNodes = h, genes = gs }
           return (inno2, innos2, genome2)

replaceGene :: Int -> Gene -> [Gene] -> [Gene]
replaceGene i g = go []
    where go _   [] = error ("Gene not found: " ++ show i)
          go acc (g1:gs)
              | innov g1 == i = reverse (g:acc) ++ gs
              | otherwise     = go (g1:acc) gs

-- Mutate weights: every weight gets a new value with some probability
-- or just a perturbation of the current value (uniform from 0 to 200%)
mutateWeights :: (RandomGen g, Monad m) => EnvParams -> Genome -> RandT g m Genome
mutateWeights env genome = do
    let (gs1, gs2) = partition enabled (genes genome)
    gs <- mapM (mutateOneWeight $ probNewWeight env) gs1 -- only the enabled genes get mutated
    return genome { genes = mergeGenes gs gs2 }

mergeGenes :: [Gene] -> [Gene] -> [Gene]
mergeGenes = go
    where go [] gs2 = gs2
          go gs1 [] = gs1
          go (g1:gs1) (g2:gs2)
              | innov g1 < innov g2 = g1 : g2 : go gs1 gs2
              | otherwise           = g2 : g1 : go gs1 gs2

mutateOneWeight :: (RandomGen g, Monad m) => Float -> Gene -> RandT g m Gene
mutateOneWeight newp gene = do
    new <- getRandomR (0, 1)
    w <- if new < newp
            then getRandomR (-weightRange, weightRange) -- new value
            else do
                wp <- getRandomR (-1, 1)
                return $! weight gene * (1 + wp) -- perturbation of old value
    return gene { weight = w }

-- Cross over depends on the fitness of the 2 genomes
crossOver :: (RandomGen g, Monad m) => EnvParams -> (Genome, Float) -> (Genome, Float) -> RandT g m Genome
crossOver env (genome1, fitness1) (genome2, fitness2) = do
    gs <- mergeGenesRandom fitness1 fitness2 (genes genome1) (genes genome2)
    let is = map inNode gs -- if we take inNode we should hit the nodes which matter
        h | null is   = 0
          | otherwise = max 0 $ maximum is - (inNodes env + outNodes env)
    return genome1 { hiddenNodes = h, genes = gs }

-- Matching genes are inherited randomly (which actually means, only weight & enabling are
-- taken randomly), while disjoint and excess genes are inherited from the fittest parent
-- (or from both when equal)
mergeGenesRandom :: (RandomGen g, Monad m) => Float -> Float -> [Gene] -> [Gene] -> RandT g m [Gene]
mergeGenesRandom fitness1 fitness2 = go
    where go [] gs2 | fitness1 > fitness2 = return []
                    | otherwise           = return gs2
          go gs1 [] | fitness2 > fitness1 = return []
                    | otherwise           = return gs1
          go (g1:gs1) (g2:gs2)
              | innov g1 < innov g2
                  = if fitness1 >= fitness2
                       then (g1 :) <$> go gs1 (g2:gs2)
                       else go gs1 (g2:gs2)
              | innov g1 > innov g2
                  = if fitness1 <= fitness2
                       then (g2 :) <$> go (g1:gs1) gs2
                       else go (g1:gs1) gs2
              | otherwise = do -- equal, i.e. matching genes: chose randomly
                  c <- uniform (False, True)
                  if c then (g1 :) <$> go gs1 gs2 else (g2 :) <$> go gs1 gs2

-- Differently from the original paper, we normalize by the sum of the genes of the 2 genomes
genomeDistance :: EnvParams -> Genome -> Genome -> Float
genomeDistance env genome1 genome2
    = (c1 env * fromIntegral ef + c2 env * fromIntegral df) / fromIntegral nf + c3 env * wf
    where a = (0, 0, 0) :: (Float, Int, Int)
          (wf, df, ef, nf) = go a (genes genome1) (genes genome2)
          go (w, d, n) [] gs2 = let k = length gs2 in (w / fromIntegral n, d, k, n + k)
          go (w, d, n) gs1 [] = let k = length gs1 in (w / fromIntegral n, d, k, n + k)
          go (w, d, n) (g1:gs1) (g2:gs2)
              | innov g1 < innov g2 = go (w, d+1, n+1) gs1 (g2:gs2)
              | innov g1 > innov g2 = go (w, d+1, n+1) (g1:gs1) gs2
              | otherwise = go (w + abs (weight g1 - weight g2), d, n+2) gs1 gs2

-- The infrastructure to keep the graph acyclic
class Connection a where
    fromNode :: a -> Int
    toNode   :: a -> Int

connectedIn :: Connection a => (Int, Int) -> [a] -> Bool
connectedIn (i, j) cs = not $ null $ (filter ((== i) . fromNode) . filter ((== j) . toNode)) cs

-- Get a set of all nodes reachable from i given the (enabled) connections
closure :: Connection a => [a] -> Int -> IntSet
closure cs i = singleton i `union` unions ds
    where ds = map (closure cs) $ map toNode $ filter ((== i) . fromNode) $ cs

-- Our connection genes are connections
instance Connection Gene where
    fromNode = inNode
    toNode   = outNode
