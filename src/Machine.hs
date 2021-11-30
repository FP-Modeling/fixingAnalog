{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}


module Machine where

import Control.Monad.Fix
import AmpOp

type Voltage = Double
type Setup a = ([Voltage], a)
newtype Machine a b = Machine {exec :: Setup a -> Setup b}

instance Functor (Machine a) where
    fmap f m = Machine $ \setup -> fmap f (exec m setup)

instance Applicative (Machine a) where
    pure x = Machine $ \_ -> pure x
    (Machine mf) <*> (Machine ms) = Machine $ \setup -> flip fmap (ms setup) =<< mf setup

instance Monad (Machine a) where
    return = pure
    (Machine m) >>= f = Machine $ \setup -> (`exec` setup) . f =<< m setup

step :: (a -> ([Voltage], a)) -> ([Voltage], a)
step operation = (voltages, next)
    where next = fix (snd . operation)
          voltages = (fst . operation) next

instance MonadFix (Machine a) where
    mfix f = Machine $ \setup -> step ((`exec` setup) . f)

ampOpInvertingM :: AmpOp -> Resistor -> Resistor -> Setup Input -> Machine Input Voltage
ampOpInvertingM model r1 r2 initialSetup =
    ($ (initialSetup, 0)) <$> mfix (\f -> Machine $ \setup -> (fst setup, \((voltages, _), counter) ->
    let (vXOld:vOutOld:_) = if counter == 0 then fst setup else voltages
        input = snd setup
        vX = (- vOutOld) / openLoopGain model
        vOut = (vXOld * (r1 + r2) - (input * r2)) / r1
    in if abs(vOutOld - vOut) <= 0.000001 && abs(vXOld - vX) <= 0.000001 then vOut else f (([vX, vOut],0), counter + 1)))

--ampOpTest :: AmpOp -> Resistor -> Resistor -> Setup Input -> Machine Input Output
ampOpTest model r1 r2 initialSetup =
    ($ initialSetup) <$> Machine (\setup -> ([0], const 0.0))

machine = Machine $ const ([0], 0.0)

--ampOpInvertingM' model r1 r2 initialSetup =
--    ($ initialSetup) <$> mfix (\f -> Machine $ \(vXOld:vOutOld:_, input) ->
--    let vX = (- vOutOld) / openLoopGain model
--        vOut = (vXOld * (r1 + r2) - input * r2) / r1
--        next =  ([vX, vOut], vOut)
--    in if abs(vOutOld - vOut) <= 0.000001 && abs(vXOld - vX) <= 0.000001 then next else f next)