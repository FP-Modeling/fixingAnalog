{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Simulation where

import AmpOp
import Circuit
import Signal
import Control.Monad.Fix

executeSimulation :: SignalState -> (SignalState -> Either String (Circuit a SignalState)) -> Signal a -> [Time] -> Either String [(Time, Output)]
executeSimulation _ _ _ [] = Right []
executeSimulation initialState buildCircuit input (t:ts) = buildCircuit initialState >>= \circuit -> do let result = circuit `simulate` input `at` t
                                                                                                        rest <- executeSimulation result buildCircuit input ts
                                                                                                        Right $ (t, getSignalOutput result `at` t) : rest


executeSimulation2 :: SignalState -> (SignalState -> Circuit a SignalState) -> Signal a -> [Time] -> [(Time, Output)]
executeSimulation2 initialState buildCircuit input samples = fix calculate (initialState, [], samples)
    where calculate f (state, l, t:ts) = if null ts then next else f (newState, next, ts)
            where newState = buildCircuit state `simulate` input `at` t
                  next = l ++ [result]
                  result = (t, getSignalOutput newState `at` t)

iSignalState = SignalState2 (dc100, ground)
c1 = ampOpInverting lm741 r1 r2
c3 = ampOpInvertingTest lm741 r1 r2

simulationSignal = executeSimulation iSignalState c1 sen [1..5]
simulationSignalTest = executeSimulation2 iSignalState c3 sen [1..5]

iState = State2 (100, 0)
c2 = ampOpInverting' lm741 r1 r2 12

simulation = c2 iState