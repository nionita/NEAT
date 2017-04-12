module Genome (
    Gene(..), Genome(..)
) where

{--
The genotype is represented as:
- the number of sensors (input), output and hidden nodes (there is always a bias "input", always 1)
- a list of connecton genes, describing the connections of the network and its weights

Connections go from one node to another with the restriction that a sensor node cannot
appear as a destination. Connections can form cycles (even with one single node).
--}

data Gene = Gene {
                inNode, outNode, innov :: Int,
                weight :: Float,
                enabled :: Bool
          }

data Genome = Genome {
                  inNodes, outNodes, hiddenNodes :: Int,
                  genes :: [Gene]
            }

{-
mutateAddConnection :: (Int, Genome) -> (Int, Genome)
mutateAddConnection (inno, genome)
-}
