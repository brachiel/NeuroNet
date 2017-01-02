module Main where

import qualified NeuroNet as N

-- Some sandbox objects to play around
wts1 = N.createWeights 5 3 [5,-2,6,2,-4,3,6,21,7,3,-1,56,3,-2,4]
bs1 = N.createBias [1,2,-3,0,1]
wts2 = N.createWeights 4 5 [-2,5,3,-1,6,32,6,1,-4,2,51,1,8,3,0,1,0,0,4,23]
bs2 = N.createBias [-1,2,-5,3]

level1 = N.Level {N.weights=wts1, N.bias=bs1}
level2 = N.Level {N.weights=wts2, N.bias=bs2}
net = [level1, level2] :: N.Net

input = N.createInput [4,2,1]

run = N.runNet net input :: N.Run

main :: IO ()
main = do putStrLn . show . N.output $ last run

