{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BlockArguments #-}
module AmpOp where

import Control.Monad.Fix

import Signal
import Circuit

type Input = Double
type Output = Double
type Resistor = Double
type OpenLoopGain = Double

dc12 :: Signal Input
dc12 = Signal $ const 12

dc14 :: Signal Input
dc14 = Signal $ const 14

dc100 :: Signal Input
dc100 = Signal $ const 100

sen :: Signal Input
sen = Signal $ \time -> 12 * sin ((pi / 2) * time)

outlet :: Signal Input
outlet = Signal $ \time -> 220 * sqrt 2 * sin (2*pi*60*time)

ground :: Signal Input
ground = Signal $ const 0

data AmpOp = AmpOp { name :: String,
                    openLoopGain :: Double,
                    manufacturer :: String
                }
           
data State = State1 Double | State2 (Double, Double)                

getOutput :: State -> Output
getOutput (State1 x) = x
getOutput (State2 (_,x)) = x

data SignalState = SignalState1 (Signal Double) | SignalState2 (Signal Double, Signal Double)

getSignalOutput :: SignalState -> Signal Output
getSignalOutput (SignalState1 x) = x
getSignalOutput (SignalState2 (_,x)) = x

instance Show AmpOp where
    show ap = Prelude.show "AmpOp => Name: " ++ Prelude.show (name ap)

lm741 :: AmpOp
lm741 = AmpOp "LM741" 2000 "Texas. Instruments"

r1 :: Resistor
r1 = 10000

r2 :: Resistor
r2 = 50000

eps = 0.000001

ampOpBuffer :: AmpOp -> SignalState -> Either String (Circuit Input SignalState)
ampOpBuffer _ (SignalState2 _) = Left "Wrong type for initial state!"
ampOpBuffer model (SignalState1 initial) =
    Right $ ($ initial) <$> mfix
    (\f -> Circuit $ \vIn -> Signal $ \time vOutOld ->
        let vOut = (/ (1 + openLoopGain model)) <$> (( * openLoopGain model) <$> vIn)
        in if abs(vOut - vOutOld) <= eps
            then SignalState1 vOut
            else f vOut)


ampOpNonInverting :: AmpOp -> Resistor -> Resistor -> SignalState -> Either String (Circuit Input SignalState)
ampOpNonInverting _ _ _ (SignalState1 _ ) = Left "Wrong type for initial state!"            
ampOpNonInverting model r1 r2 (SignalState2 initial) =
    Right $ ($ initial) <$> mfix
    (\f -> Circuit $ \vIn -> Signal $ \time (vXOld, vOutOld) ->
        let vX = (/ openLoopGain model) <$> (((* openLoopGain model) <$> vIn) - vOutOld)
            vOut = (/ r1) <$> ((* (r1 + r2)) <$> vXOld)
        in if abs(vOut - vOutOld) <= eps && abs(vX - vXOld) <= eps
            then SignalState2 (vX, vOut)
            else f (vX, vOut))

ampOpInverting :: AmpOp -> Resistor -> Resistor -> SignalState -> Either String (Circuit Input SignalState)
ampOpInverting _ _ _ (SignalState1 _ ) = Left "Wrong type for initial state!"
ampOpInverting model r1 r2 (SignalState2 initial) =
    Right $ ($ initial) <$> mfix
    (\f -> Circuit $ \vIn -> Signal $ \time (vXOld, vOutOld) ->
        let vX = (/ openLoopGain model) <$> (- vOutOld)
            vOut = (/ r1) <$> (((* (r1 + r2)) <$> vXOld) - ((* r2) <$> vIn))
        in if abs(vOutOld - vOut) <= eps && abs(vXOld - vX) <= eps
            then SignalState2 (vX, vOut)
            else f (vX, vOut))                     

-- Calculating the output given a fixed input, to demonstrate the fix point only across the iteration axis
ampOpInverting' :: AmpOp -> Resistor -> Resistor -> Input -> State -> Either String Output
ampOpInverting' _ _ _ _ (State1 _) = Left "Wrong type for initial state!"
ampOpInverting' model r1 r2 input (State2 initial) = Right $ fix calculate initial
    where calculate f (vXOld, vOutOld) = if abs(vOutOld - vOut) <= 0.000001 && abs(vXOld - vX) <= 0.000001 then vOut else f (vX, vOut)
            where vX = (- vOutOld) / openLoopGain model
                  vOut = (vXOld * (r1 + r2) - (input * r2)) / r1
