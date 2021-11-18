module Simulation where

import AmpOp
import Circuit
import Signal

executeSimulation :: SignalState -> (SignalState -> Either String (Circuit a SignalState)) -> Signal a -> [Time] -> Either String [(Time, Output)]
executeSimulation _ _ _ [] = Right []
executeSimulation initialState buildCircuit input (t:ts) = buildCircuit initialState >>= \circuit -> do let result = circuit `simulate` input `at` t
                                                                                                        rest <- executeSimulation result buildCircuit input ts
                                                                                                        Right $ (t, getSignalOutput result `at` t) : rest
                              
iSignalState = SignalState2 (dc100, ground)
c1 = ampOpInverting lm741 r1 r2

simulationSignal = executeSimulation iSignalState c1 sen [1..100]

iState = State2 (100, 0)
c2 = ampOpInverting' lm741 r1 r2 12

simulation = executeSimulation' iState c2

executeSimulation' :: State -> (State -> Either String Output) -> Either String Output
executeSimulation' initialState buildCircuit = buildCircuit initialState >>= Right