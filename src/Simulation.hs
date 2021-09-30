module Simulation where

import AmpOp
import Circuit
import Signal

type Source = Signal Input

executeSimulation :: Circuit Input Output -> Source -> [Time] -> [(Time, Output)]
executeSimulation circuit input samples = do time <- samples
                                             let result = (circuit `simulate` input) `at` time
                                             return (time, result)                                           
