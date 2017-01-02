module Backprop where

import qualified Data.Matrix as M
import NeuroNet

type Error = Output

costFunction :: Output -> Output -> Double
costFunction out goal = (sum $ fmap (**2) (out - goal)) / 2       -- 1/2 * Sum_i (o_i-g_i)^2

costFunction' :: Output -> Output -> Output
costFunction' out goal = out - goal         -- o_i - g_i

outputError :: LevelRun -> Output -> Error
outputError lrun goal = M.elementwise (*) (costFunction' (output lrun) goal) (sigmoidO' $ wInput lrun)


