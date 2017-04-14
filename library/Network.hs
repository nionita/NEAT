module Network (
  genomeToFunction
) where

import Data.Array
import qualified Data.IntMap as M
import Genome

type Network = [Float] -> [Float]

-- Our network is a function from the list of inputs to the list of outputs
genomeToFunction :: EnvParams -> Genome -> [Float] -> [Float]
genomeToFunction env genome is = map (a!) [n1..n2]
    where f gene imap = let nw = [(inNode gene, weight gene)]
                        in M.insertWith (++) (outNode gene) nw imap
          rmap = foldr f M.empty $ filter enabled $ genes genome
          n  = inNodes env + outNodes env + hiddenNodes genome -- last node number
          n1 = inNodes env + 1            -- first output node number
          n2 = inNodes env + outNodes env -- last output node number
          g (i, nws) = let (ns, ws) = unzip nws
                           as = map (a!) ns
                           ps = zipWith (*) as ws
                           x  = sigmoid $ sum ps
                       in (i, x)
          a = array (0, n) $ [(0, 1)] ++ zip [1..] is ++ map g (M.assocs rmap)

sigmoid :: Float -> Float
sigmoid x = 1 / (1 + exp (-x))
