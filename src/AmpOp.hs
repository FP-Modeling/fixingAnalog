{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BlockArguments #-}
module AmpOp where

import Control.Monad.Fix

import Signal
import Circuit
import Data.Complex

type Input = Complex Double
type Output = Complex Double
type Value = Double
type Impedance = Signal (Complex Double)

dc12 :: Signal Input
dc12 = Signal $ const 12

dc14 :: Signal Input
dc14 = Signal $ const 14

dc100 :: Signal Input
dc100 = Signal $ const 100

sen :: Signal Input
sen = Signal $ \time -> 12 * sin ((pi / 2) * time)

outlet :: Signal Input
outlet = Signal $ \time -> 220 * sqrt 2 * sin (2 * pi * 60 * time)

ground :: Signal Input
ground = Signal $ const 0

data AmpOp = AmpOp { name :: String,
                     openLoopGain :: Signal (Complex Double),
                     manufacturer :: String
                }
           
data State =
  OneNode (Complex Double) |
  TwoNodes (Complex Double, Complex Double) |
  ThreeNodes (Complex Double, Complex Double, Complex Double) |
  FourNodes (Complex Double, Complex Double, Complex Double, Complex Double)

getOutput :: State -> Output
getOutput (OneNode x) = x
getOutput (TwoNodes (_,x)) = x
getOutput (ThreeNodes (_,_,x)) = x
getOutput (FourNodes (_,_,_,x)) = x

data SignalState =
  SignalOneNode (Signal (Complex Double)) |
  SignalTwoNodes (Signal (Complex Double), Signal (Complex Double)) |
  SignalThreeNodes (Signal (Complex Double), Signal (Complex Double), Signal (Complex Double)) |
  SignalFourNodes (Signal (Complex Double), Signal (Complex Double), Signal (Complex Double), Signal (Complex Double))
  deriving (Show)

getSignalOutput :: SignalState -> Signal Output
getSignalOutput (SignalOneNode x) = x
getSignalOutput (SignalTwoNodes (_,x)) = x
getSignalOutput (SignalThreeNodes (_,_,x)) = x
getSignalOutput (SignalFourNodes (_,_,_,x)) = x

instance Show AmpOp where
    show ap = Prelude.show "AmpOp => Name: " ++ Prelude.show (name ap)

lm741 :: AmpOp
lm741 = AmpOp "LM741" 200000 "Texas. Instruments"

r1 :: Impedance
r1 = 10000

r2 :: Impedance
r2 = 50000

r3 :: Impedance
r3 = 20000

r4 :: Impedance
r4 = 30000

eps = 0.000001

ampOpBuffer :: AmpOp -> SignalState -> Either String (Circuit Input SignalState)
ampOpBuffer model (SignalOneNode initial) =
    Right $ ($ initial) <$> mfix
    (\f -> Circuit $ \vIn -> Signal $ \time vOutOld ->
        let vOut = (vIn * openLoopGain model) / (1 + openLoopGain model)
        in if abs((realPart <$> vOut) - (realPart <$> vOutOld)) <= eps
            then SignalOneNode vOut
            else f vOut)
ampOpBuffer _ _ = Left "Wrong type for initial state!"

ampOpNonInverting :: AmpOp -> Impedance -> Impedance -> SignalState -> Either String (Circuit Input SignalState)
ampOpNonInverting model r1 r2 (SignalTwoNodes initial) =
    Right $ ($ initial) <$> mfix
    (\f -> Circuit $ \vIn -> Signal $ \time (vXOld, vOutOld) ->
        let vX = ((vIn * openLoopGain model) - vOutOld) / openLoopGain model
            vOut = (vXOld * (r1 + r2)) / r1
        in if abs((realPart <$> vOut) - (realPart <$> vOutOld)) <= eps && abs((realPart <$> vX) - (realPart <$> vXOld)) <= eps
            then SignalTwoNodes (vX, vOut)
            else f (vX, vOut))
ampOpNonInverting _ _ _ _ = Left "Wrong type for initial state!"            

ampOpInverting :: AmpOp -> Impedance -> Impedance -> SignalState -> Either String (Circuit Input SignalState)
ampOpInverting model r1 r2 (SignalTwoNodes initial) =
    Right $ ($ initial) <$> mfix
    (\f -> Circuit $ \vIn -> Signal $ \time (vXOld, vOutOld) ->
        let vX = (- vOutOld) / openLoopGain model
            vOut = ((vXOld * (r1 + r2)) - (vIn * r2)) / r1
        in if abs((realPart <$> vOutOld) - (realPart <$> vOut)) <= eps && abs((realPart <$> vXOld) - (realPart <$> vX)) <= eps
            then SignalTwoNodes (vX, vOut)
            else f (vX, vOut))
ampOpInverting _ _ _ _ = Left "Wrong type for initial state!"


-- ampOpSpecial :: AmpOp -> Impedance -> Impedance -> Impedance -> Impedance -> SignalState -> Either String (Circuit Input SignalState)
-- ampOpSpecial model z1 z2 z3 z4 (SignalThreeNodes initial) =
--   Right $ ($ initial) <$> mfix
--   (\f -> Circuit $ \vIn -> Signal $ \time (vXOld, vKOld, vOutOld) ->
--       let vK = vOutOld + (vOutOld / openLoopGain model)
--           vX = ((z2 + z3) * vKOld) / z3
--           vOut = (((z2 * z4 + z1 * z4 + z1 * z2) * vXOld) - z1 * z4 * vKOld - z2 * z4 * vIn) / (z1 * z2)
--       in if abs((realPart <$> vOutOld) - (realPart <$> vOut)) <= eps
--           && abs((realPart <$> vKOld) - (realPart <$> vK)) <= eps
--           && abs((realPart <$> vXOld) - (realPart <$> vX)) <= eps
--           then SignalThreeNodes (vX, vK, vOut)
--           else f (vX, vK, vOut))

  
ampOpInvertingTest :: AmpOp -> Impedance -> Impedance -> SignalState -> Circuit Input SignalState
ampOpInvertingTest model r1 r2 (SignalTwoNodes initial) =
    ($ initial) <$> mfix
    (\f -> Circuit $ \vIn -> Signal $ \time (vXOld, vOutOld) ->
        let vX = (- vOutOld) / openLoopGain model
            vOut = ((vXOld * (r1 + r2)) - (vIn * r2)) / r1
        in if abs((realPart <$> vOutOld) - (realPart <$> vOut)) <= eps && abs((realPart <$> vXOld) - (realPart <$> vX)) <= eps
            then SignalTwoNodes (vX, vOut)
            else f (vX, vOut))                    

-- Calculating the output given a fixed input, to demonstrate the fix point only across the iteration axis
ampOpInverting' :: AmpOp -> Impedance -> Impedance -> Input -> State -> Either String Output
ampOpInverting' _ _ _ _ (OneNode _) = Left "Wrong type for initial state!"
ampOpInverting' model r1 r2 input (TwoNodes initial) = Right $ fix calculate initial
    where calculate f (vXOld, vOutOld) = if abs(realPart vOutOld - realPart vOut) <= 0.000001 && abs(realPart vXOld - realPart vX) <= 0.000001 then vOut else f (vX, vOut)
            where vX = (- vOutOld) / (openLoopGain model `at` 0)
                  vOut = (vXOld * (r1 + r2) `at` 0 - (input * r2 `at` 0)) / r1 `at` 0
