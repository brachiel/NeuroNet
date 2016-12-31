module NeuroNet where

import Data.Matrix as M

type DMatrix = M.Matrix Double

type Matrix = DMatrix
type Input = DMatrix
type Output = DMatrix

data Level = Level {weights::DMatrix, bias::DMatrix}
type Net = [Level]


createInput :: [Double] -> Input
createInput list = M.fromList (length list) 1 list

applyLevel :: Input -> Level -> Output
applyLevel input level = M.elementwise (+) a b
                         where a = M.multStd2 (weights level) input
                               b = (bias level)

