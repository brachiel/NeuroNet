module Main where

import NeuroNet

-- Some sandbox objects to play around
bs = createBias [1,2,3,4,1]
wts = createWeights 5 3 [5,2,6,2,4,3,6,21,7,3,1,56,3,2,4]
input = createInput [4,2,1]

level = Level {weights=wts, bias=bs}
output = applyLevel level input

main :: IO ()
main = do putStrLn $ show output

