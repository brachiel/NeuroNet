module NeuroNet where

import qualified Data.Matrix as M

type DMatrix = M.Matrix Double

type Weights = DMatrix
type Bias = DMatrix
type Input = DMatrix
type Output = DMatrix

data Level = Level {weights::Weights, bias::Bias}
type Net = [Level]
data LevelRun = LevelRun {input::Input, output::Output, wInput::Input}
type Run = [LevelRun]

-- | Convert a list of doubles to an input vector
createInput :: [Double] -> Input
createInput list = M.fromList (length list) 1 list

-- | Convert a list of doubles to a bias vector
createBias :: [Double] -> Bias
createBias = createInput

-- | Convert a list of double to a system matrix of a neuron level
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

-- | The sigmoid activation function applied to a whole output layer
sigmoidO :: Output -> Output
sigmoidO out = fmap sigmoid out

-- | The derivative of sigmoid activation function applied to a whole output layer
sigmoidO' :: Output -> Output
sigmoidO' out = fmap sigmoid' out

-- | Apply a layer and the activation function to an input; gives an output
applyLevel :: Level -> Input -> Output
applyLevel l = sigmoidO . bareApplyLevel l

-- | Apply the system matrix of the layer and the bias to the input
bareApplyLevel :: Level -> Input -> Output
bareApplyLevel level input = M.elementwise (+) a b
                         where a = M.multStd2 (weights level) input
                               b = (bias level)

-- | Apply a network to an input
runNet :: Net -> Input -> Run
runNet [] _ = []
runNet (level:levels) input = [LevelRun {input=input, output=output, wInput=wInput}] ++ runNet levels output
                                    where wInput = bareApplyLevel level input
                                          output = sigmoidO wInput

