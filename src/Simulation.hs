{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Simulation where

import AmpOp
    ( SignalState(..),
      State(TwoNodes),
      Output,
      Value,
      dc100,
      dc12,
      sen,
      ground,
      getSignalOutput,
      lm741,
      r1,
      r2,
      r3,
      r4,
      -- ampOpSpecial,
      ampOpBuffer,
      ampOpInverting,
      ampOpInvertingTest,
      ampOpInverting' )
import Circuit
import Signal
import Control.Monad.Fix
import Data.Complex

executeTimeSimulation :: SignalState -> (SignalState -> Either String (Circuit a SignalState)) -> Signal a -> [Metric] -> Either String [(Time, Value)]
executeTimeSimulation _ _ _ [] = Right []
executeTimeSimulation initialState buildCircuit input (t:ts) = buildCircuit initialState >>= \circuit -> do let result = circuit `simulate` input `at` t
                                                                                                            rest <- executeTimeSimulation result buildCircuit input ts
                                                                                                            Right $ (realPart t, realPart (getSignalOutput result `at` t)) : rest

executeTimeSimulation2 :: SignalState -> (SignalState -> Circuit a SignalState) -> Signal a -> [Metric] -> [(Metric, Output)]
executeTimeSimulation2 initialState buildCircuit input samples = fix calculate (initialState, [], samples)
    where calculate f (state, l, t:ts) = if null ts then next else f (newState, next, ts)
            where newState = buildCircuit state `simulate` input `at` t
                  next = l ++ [result]
                  result = (t, getSignalOutput newState `at` t)

iSignalState = SignalTwoNodes (dc100, ground)
iSignalState3 = SignalThreeNodes (dc100, dc100, dc100)
iSignalBuffer = SignalOneNode dc100
c0 = ampOpBuffer lm741
c1 = ampOpInverting lm741 r1 r2
c3 = ampOpInvertingTest lm741 r1 r2
-- cS = ampOpSpecial lm741 r1 r2 r3 r4

simulationSignal = executeTimeSimulation iSignalState c1 sen (map (:+ 0) [1..5])
simulationSignalTest = executeTimeSimulation2 iSignalState c3 sen (map (:+ 0) [1..5])
-- simulationSpecial = executeTimeSimulation iSignalState3 cS dc12 (map (:+ 0) [1])

iState = TwoNodes (100, 0)
c2 = ampOpInverting' lm741 r1 r2 12

simulation = c2 iState
