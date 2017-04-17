module Network (
  Network,
  genomeToFunction
) where

import Data.Array
import qualified Data.IntMap as M
import Genome

type Network = [Float] -> [Float]

-- Our network is a function from the list of inputs to the list of outputs
genomeToFunction :: EnvParams -> Genome -> Network
genomeToFunction env genome is
    | map fst axs /= [0..n] = error $ "Undefined indices: n = " ++ show n ++ ", axs = " ++ show axs
                                      ++ "\n" ++ show genome
    | otherwise             = map (a!) [n1..n2]
    where f gene imap = let nw = [(inNode gene, weight gene)]
                        in M.insertWith (++) (outNode gene) nw imap
          rmap = foldr f M.empty $ filter enabled $ genes genome
          n  = envInNodes env + envOutNodes env + hiddenNodes genome -- last node number
          n1 = envInNodes env + 1               -- first output node number
          n2 = envInNodes env + envOutNodes env -- last output node number
          g (i, nws) = let (ns, ws) = unzip nws
                           as = map (a!) ns
                           ps = zipWith (*) as ws
                           x  = sigmoid $ sum ps
                       in (i, x)
          axs = [(0, 1)] ++ zip [1..] is ++ merge (map g (M.assocs rmap)) (zip [n1..n] $ repeat 0)
          a = array (0, n) axs

merge :: Ord a => [(a, b)] -> [(a, b)] -> [(a, b)]
merge = go
    where go [] as = as
          go as [] = as
          go pss1@(p1@(a1,_):ps1) pss2@(p2@(a2,_):ps2)
              | a1 < a2   = p1 : go ps1 pss2
              | a1 > a2   = p2 : go pss1 ps2
              | otherwise = p1 : go ps1  ps2

sigmoid :: Float -> Float
sigmoid x = 1 / (1 + exp (-x))
