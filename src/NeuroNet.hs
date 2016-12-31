module NeuroNet where

import Data.Matrix as M

type DMatrix = M.Matrix Double

type Weights = DMatrix
type Bias = DMatrix
type Input = DMatrix
type Output = DMatrix

data Level = Level {weights::Weights, bias::DMatrix}
type Net = [Level]


createInput :: [Double] -> Input
createInput list = M.fromList (length list) 1 list

createBias :: [Double] -> Bias
createBias = createInput

createWeights :: Int -> Int -> [Double] -> Weights
createWeights rows cols wts = M.fromList rows cols wts

-- | The sigmoid activation function, a standard activation function defined
--   on the range (0, 1).
sigmoid :: (Floating a) => a -> a
sigmoid t = 1 / (1 + exp (-1 * t))

-- | The derivative of the sigmoid function conveniently can be computed in
--   terms of the sigmoid function.
sigmoid' :: (Floating a) => a -> a
sigmoid' t = s * (1 - s)
              where s = sigmoid t

sigmoidO :: Output -> Output
sigmoidO out = fmap sigmoid out

applyLevel :: Level -> Input -> Output
applyLevel l = sigmoidO . preApplyLevel l

preApplyLevel :: Level -> Input -> Output
preApplyLevel level input = M.elementwise (+) a b
                         where a = M.multStd2 (weights level) input
                               b = (bias level)

