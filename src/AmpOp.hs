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

outlet :: Signal Input
outlet = Signal $ \time -> 220 * sqrt 2 * (sin 2*pi*60*time)

ground :: Signal Input
ground = Signal $ const 0

data AmpOp = AmpOp { name :: String,
                    openLoopGain :: Double,
                    manufacturer :: String
                }

instance Show AmpOp where
    show ap = Prelude.show "AmpOp => Name: " ++ Prelude.show (name ap)

lm741 :: AmpOp
lm741 = AmpOp "LM741" 200000 "Texas. Instruments"

r1 :: Resistor
r1 = 10000

r2 :: Resistor
r2 = 50000

eps = 0.0001

makeAmpOpModel :: String -> OpenLoopGain -> String -> AmpOp
makeAmpOpModel = AmpOp

ampOpBuffer :: AmpOp -> Circuit Input Output
ampOpBuffer model =
    ($ ground) <$> mfix
    (\f -> Circuit $ \vIn -> Signal $ \time vOutOld ->
        let vOut = (/ (1 + openLoopGain model)) <$> (( * openLoopGain model) <$> vIn)
        in if vOut - vOutOld <= eps
            then vOut `at` time
            else f vOut)

ampOpNonInverting :: AmpOp -> Resistor -> Resistor -> Circuit Input Output
ampOpNonInverting model r1 r2 =
    ($ (ground, ground)) <$> mfix
    (\f -> Circuit $ \vIn -> Signal $ \time (vXOld, vOutOld) ->
        let vX = (/ openLoopGain model) <$> (((* openLoopGain model) <$> vIn) - vOutOld)
            vOut = (/ r1) <$> ((* (r1 + r2)) <$> vXOld)
        in if vOut - vOutOld <= eps && vX - vXOld <= eps
            then vOut `at` time
            else f (vX, vOut))

ampOpInverting :: AmpOp -> Resistor -> Resistor -> Circuit Input Output
ampOpInverting model r1 r2 =
    ($ (ground, ground)) <$> mfix
    (\f -> Circuit $ \vIn -> Signal $ \time (vXOld, vOutOld) ->
        let vX = (/ openLoopGain model) <$> (- vOutOld)
            vOut = (/ r1) <$> (((* (r1 + r2)) <$> vXOld) - ((* r2) <$> vIn))
        in if vOutOld - vOut <= eps && vXOld - vX <= eps
            then vOut `at` time
            else f (vX, vOut))            

-- Calculating the output given a fixed input, to demonstrate the fix point only across the iteration axis
ampOpInverting' :: AmpOp -> Input -> Resistor -> Resistor -> Output
ampOpInverting' model input r1 r2 = fix calculate (0, 0)
    where calculate f (vOutOld, vXOld) = if vOutOld - vOut <= 0.0001 && vXOld - vX <= 0.0001 then vOut else f (vOut, vX)
            where vX = (- vOutOld) / openLoopGain model
                  vOut = (vXOld * (r1 + r2) - (input * r2)) / r1
