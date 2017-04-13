module Genome (
    Gene(..), Genome(..), Connection(..), Innovations,
    mutateAddConnection, mutateAddNode, mutateWeights,
    closure, connectedIn
) where

import Control.Monad.Random
import Data.IntSet (IntSet, member, union, unions, singleton)
import Data.List (partition)

{--
The genotype is represented as:
- the number of sensors (inputs), outputs and hidden nodes (there is always a bias "input", always 1)
- a list of connecton genes, describing the connections of the network and its weights

Connections go from one node to another with 2 restrictions:
- a sensor node cannot appear as a destination
- connections cannot form cycles
--}

data Gene = Gene {
                inNode, outNode, innov :: Int,
                weight :: Float,
                enabled :: Bool
          } deriving Show

data Genome = Genome {
                  inNodes, outNodes, hiddenNodes :: Int,
                  genes :: [Gene]
            } deriving Show

type Innovations = [((Int, Int), Int)]

weightRange :: Float
weightRange = 10  -- value range for new weights

-- Reusing the innovation numbers facilitates the crossovers
reuseInnovation :: Int -> Int -> (Int, Innovations) -> (Int, (Int, Innovations))
reuseInnovation f t (inno, innos)
    = case lookup (f, t) innos of
          Nothing -> (inno, (inno + 1, ((f, t), inno) : innos)) -- consume
          Just iv -> (iv,   (inno, innos))                      -- reuse

-- We have to add connections in such a way that we do not create cycles
-- If we have chosen the wrong nodes, we may fail
-- Same innovation in the same generation will get the same number (reuse)
mutateAddConnection :: (RandomGen g, Monad m) => Int -> Innovations -> Genome
                                              -> RandT g m (Maybe (Int, Innovations, Genome))
mutateAddConnection inno innos genome = do
    let maf = inNodes genome + outNodes genome + hiddenNodes genome
        mit = inNodes genome + 1
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
mutateAddNode :: (RandomGen g, Monad m) => Int -> Innovations -> Genome
                                        -> RandT g m (Int, Innovations, Genome)
mutateAddNode inno innos genome = do
    let cs = filter enabled (genes genome)
    co <- uniform cs -- what if all genes are disabled??
    let cd = co { enabled = False }
        h = hiddenNodes genome + 1
        n = inNodes genome + outNodes genome + h
        (innog1, (inno1, innos1)) = reuseInnovation (inNode co) n  (inno,  innos)
        (innog2, (inno2, innos2)) = reuseInnovation n (outNode co) (inno1, innos1)
        gene1 = co { outNode = n, innov = innog1, weight = 1 }
        gene2 = co { inNode = n,  innov = innog2 }
        gs = insertGene gene1 $ insertGene gene2 $ replaceGene (innov co) cd (genes genome)
        genome2 = genome { hiddenNodes = h, genes = gs }
    return (inno2, innos2, genome2)

replaceGene :: Int -> Gene -> [Gene] -> [Gene]
replaceGene i g = go []
    where go acc [] = error ("Gene not found: " ++ show i)
          go acc (g1:gs)
              | innov g1 == i = reverse (g:acc) ++ gs
              | otherwise     = go (g1:acc) gs

-- Mutate weights: every weight gets a new value with some probability
-- or just a perturbation of the current value (uniform from 0 to 200%)
mutateWeights :: (RandomGen g, Monad m) => Float -> Genome -> RandT g m Genome
mutateWeights newp genome = do
    let (gs1, gs2) = partition enabled (genes genome)
    gs <- mapM (mutateOneWeight newp) gs1 -- only the enabled genes get mutated
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
    w <- if new >= newp
            then getRandomR (-weightRange, weightRange) -- new value
            else do
                wp <- getRandomR (-1, 1)
                return $! weight gene * (1 + wp) -- perturbation of old value
    return gene { weight = w }

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
