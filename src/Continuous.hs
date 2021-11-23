module Continuous where

import Circuit
import AmpOp
import Signal

type CircuitBuilder a b = (b, b -> Circuit a b)

buildCircuit :: CircuitBuilder a b -> Circuit a b
buildCircuit (initialState, builder) = builder initialState


