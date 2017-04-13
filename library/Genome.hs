module Genome (
    Gene(..), Genome(..),
    mutateAddConnection, mutateAddNode
) where

import Control.Monad.Random
import qualified Data.IntSet as S

{--
The genotype is represented as:
- the number of sensors (input), output and hidden nodes (there is always a bias "input", always 1)
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

-- We have to add connections in such a way that we do not create cycles
mutateAddConnection :: (RandomGen g, Monad m) => Int -> Genome -> RandT g m (Maybe Genome)
mutateAddConnection inno genome = do
    let maf = inNodes genome + outNodes genome + hiddenNodes genome
        mit = inNodes genome + 1
    f <- getRandomR (0, maf)   -- every node can be source
    t <- getRandomR (mit, maf) -- bias and inputs can't be destination
    if f == t || (f, t) `isIn` (genes genome) || S.member f (closure (genes genome) t)
       then return Nothing
       else do
           w <- getRandomR (-1, 1)  -- which range should we take?
           let genome2 = genome { genes = genes genome ++ [gene] }
               gene = Gene { inNode = f, outNode = t, innov = inno, weight = w, enabled = True }
           return (Just genome2)

isIn :: (Int, Int) -> [Gene] -> Bool
isIn (i, j) gs = not $ null $ (filter ((== i) . inNode) . filter ((== j) . outNode)) gs

closure :: [Gene] -> Int -> S.IntSet
closure gs i = S.singleton i `S.union` S.unions ds
    where ds = map (closure gs) $ map outNode $ filter ((== i) . inNode) $ filter enabled $ gs

mutateAddNode :: (RandomGen g, Monad m) => Int -> Genome -> RandT g m Genome
mutateAddNode inno genome = do
    let cs = filter enabled (genes genome)
    co <- uniform cs
    let cd = co { enabled = False }
        (cs1, cs2) = span ((/= innov co) . innov) (genes genome)
        h = hiddenNodes genome + 1
        gene1 = co { outNode = h, innov = inno, weight = 1 }
        gene2 = co { inNode = h, innov = inno + 1 }
        genome2 = genome { hiddenNodes = h, genes = cs1 ++ [cd] ++ tail cs2 ++ [gene1, gene2] }
    return genome2
