module Simulation where

import AmpOp
import Circuit
import Signal

type Input = Signal In
type Output = Signal Out

executeSimulation :: Circuit In Out -> Input -> [Time] -> [Out]
executeSimulation circuit input times = do sample <- times
                                           let result = (circuit `simulate` input) `at` sample
                                           return result                                           