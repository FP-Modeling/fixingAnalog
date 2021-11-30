{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}


module Machine2 where

import Control.Monad.Fix
import AmpOp

import Machine


type Setup2 a = (Voltage, [a])
newtype Machine2 a b = Machine2 {exec2 :: Setup2 a -> Setup2 b}

instance Functor (Machine2 a) where
    fmap f m = Machine2 $ \setup -> fmap (fmap f) (exec2 m setup)

--instance Applicative (Machine2 a) where
--    pure x = Machine2 $ const (0.0, [x])
--    (Machine2 mf) <*> (Machine2 ms) = Machine2 $ \setup -> mf setup >>= \r -> fmap r ms setup
    --(Machine2 mf) <*> (Machine2 ms) = Machine2 $ \setup -> flip fmap (ms setup) =<< mf setup

--instance Monad (Machine2 a) where
--    return = pure
--    (Machine2 m) >>= f = Machine2 $ \setup -> (`exec2` setup) . f =<< m setup

--step :: (a -> ([Voltage], a)) -> ([Voltage], a)
--step operation = (voltages, next)
    --where next = fix (snd . operation)
          --voltages = (fst . operation) next

--instance MonadFix (Machine a) where
    --mfix f = Machine $ \setup -> step ((`exec` setup) . f)

--ampOpInvertingM :: AmpOp -> Resistor -> Resistor -> Setup Input -> Machine Input [Voltage]
--ampOpInvertingM model r1 r2 initialSetup =
--    ($ (initialSetup, 0)) <$> mfix (\f -> Machine $ \setup -> ((:),) (\((voltages, _), counter) ->
--    let (vXOld:vOutOld:_) = if counter == 0 then fst setup else voltages
--        input = snd setup
--        vX = (- vOutOld) / openLoopGain model
--        vOut = (vXOld * (r1 + r2) - (input * r2)) / r1
--    in if abs(vOutOld - vOut) <= 0.000001 && abs(vXOld - vX) <= 0.000001 then vOut else f (([vX, vOut], input), counter + 1)) (const (snd setup)))

--ampOpTest :: AmpOp -> Resistor -> Resistor -> Setup Input -> Machine Input Output
--ampOpTest model r1 r2 initialSetup =
--    ($ initialSetup) <$> mfix (\f -> Machine $ \setup -> ([0], const 0.0))

--machine = Machine $ const ([0], 0.0)

--ampOpInverting' :: AmpOp -> Resistor -> Resistor -> Input -> State -> Either String Output
--ampOpInverting' _ _ _ _ (State1 _) = Left "Wrong type for initial state!"
--ampOpInverting' model r1 r2 input (State2 initial) = Right $ fix calculate initial
    --where calculate f (vXOld, vOutOld) = if abs(vOutOld - vOut) <= 0.000001 && abs(vXOld - vX) <= 0.000001 then vOut else f (vX, vOut)
            --where vX = (- vOutOld) / openLoopGain model
                    --vOut = (vXOld * (r1 + r2) - (input * r2)) / r1