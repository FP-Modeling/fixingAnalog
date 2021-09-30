module Simulation where

import AmpOp
import Circuit
import Signal

type Source = Signal Input

executeSimulation :: Circuit Input Output -> Source -> [Time] -> [Output]
executeSimulation circuit input times = do sample <- times
                                           let result = (circuit `simulate` input) `at` sample
                                           return result                                           