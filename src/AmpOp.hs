{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NamedFieldPuns #-}
module AmpOp where

import Control.Monad.Fix

import Signal
import Circuit

type Input = Signal Double
type Output = Signal Double
type Resistor = Signal Double
type OpenLoopGain = Signal Double

dc12 :: Signal Double
dc12 = Signal $ const 12

outlet :: Signal Double
outlet = Signal $ \time -> 220 * sqrt 2 * (sin 2*pi*60*time)

ground :: Signal Double
ground = Signal $ const 0

data AmpOp = AmpOp { name :: String,
                    openLoopGain :: Signal Double,
                    manufacturer :: String
                }

instance Show AmpOp where
    show ap = Prelude.show "AmpOp => Name: " ++ Prelude.show (name ap)

lm741 :: AmpOp
lm741 = AmpOp "LM741" 200000 "Texas. Instruments"

makeOpenLoopGain :: Double -> OpenLoopGain
makeOpenLoopGain = pure

a0 :: OpenLoopGain
a0 = Signal $ const 100

makeResistor :: Double -> Resistor
makeResistor = pure

r1 :: Resistor
r1 = Signal $ const 10000

r2 :: Resistor
r2 = Signal $ const 50000

makeAmpOpModel :: String -> OpenLoopGain -> String -> AmpOp
makeAmpOpModel = AmpOp

-- ampOp :: Input -> Input -> AmpOp -> Output
-- ampOp (Circuit vPlus) (Circuit vMinus) model = return $ openLoopGain model * (vPlus  - vMinus)

ampOpBuffer :: AmpOp -> (Input -> Time -> Output)
ampOpBuffer model input time = (circuit `simulate` input) `at` time
    where circuit = ($ ground)
                   <$> mfix (\f -> Circuit $ \vIn -> Signal $ \ time vOut -> if (openLoopGain model  * vIn) / (1 + openLoopGain model) - vOut <= 0.0001 then
                        vOut else
                        f (openLoopGain model  * vIn / (1 + openLoopGain model)))

--ampBuffer :: AmpOp -> Circuit Double Double
ampBuffer model =
  ($ ground) <$>  mfix
  (\f -> Circuit $ \vIn -> Signal $ \t vOutOld ->
    let vOut = (openLoopGain model * vIn) / (1 + openLoopGain model)
        eps = 1e-3
    in if vOut - vOutOld <= eps
       then vOut
       else f (vOut))
    


ampOpNonInverting :: AmpOp -> Resistor -> Resistor -> (Input -> Time -> Output)
ampOpNonInverting model r1 r2 input time= snd $ (circuit `simulate` input) `at` time
    where circuit = ($ (ground, ground))
                    <$> mfix (\f -> Circuit $ \vIn -> Signal $ \ time (vX, vOut) -> if (((openLoopGain model * vIn) - vOut) / openLoopGain model) - vX <= 0.001 && ((vX * (r1 + r2)) / r1) - vOut <= 0.01 then
                        (vX, vOut) else
                        f (((openLoopGain model * vIn) - vOut) / openLoopGain model, (vX * (r1 + r2)) / r1))

-- ampOpNonInverting :: Input -> Resistor -> Resistor -> AmpOp -> Output
-- ampOpNonInverting vIn r1 r2 model = mdo vX <- Circuit ((vOut * r1) / (r1 - r2))
--                                         vOut <- ampOp vIn (Circuit vX) model
--                                         return vOut                               

ampOpInverting :: AmpOp -> Resistor -> Resistor -> (Input -> Time -> Output)
ampOpInverting model r1 r2 input time = snd $ (circuit `simulate` input) `at` time
    where circuit = ($ (ground, ground))
                    <$> mfix (\f -> Circuit $ \vIn -> Signal $ \ time (vX, vOut) -> if vX - (- vOut) / openLoopGain model <= 0.001 && vOut - ((vX * (r1 + r2)) - vIn * r2) / r1 <= 0.01 then
                        (vX, vOut) else
                        f ((- vOut) / openLoopGain model, (vX * (r1 + r2) - vIn * r2) / r1))


-- ampOpInverting (Circuit vIn) model r1 r2 = Circuit vOut
--     where Circuit (_,_,vOut,_,_,_) = ($ (vIn, ground, ground, model, r1, r2)) 
--                 <$> mfix (\f -> Circuit $ \(vIn, vX, vOut, model, r1, r2) -> if (vX - (- vOut) / openLoopGain model <= 0.001) && (vOut - ((vX * (r1 + r2)) - vIn * r2) / r1 <= 0.01) then
--                                                                     (vIn, vX, vOut, model, r1, r2) else
--                                                                     f (vIn, (- vOut) / openLoopGain model, ((vX * (r1 + r2)) - vIn * r2) / r1, model, r1, r2))






-- func f = Circuit $ \vIn -> Signal $ \ time (vX, vOut) -> if (vX - (- vOut) / openLoopGain model <= 0.001) && (vOut - ((vX * (r1 + r2)) - vIn * r2) / r1 <= 0.01) then
--                                                       (vX, vOut) else
--                                                       f ((- vOut) / openLoopGain model, ((vX * (r1 + r2)) - vIn * r2) / r1)
