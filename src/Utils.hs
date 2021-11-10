{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils where
import Control.Monad.Fix
import System.IO
import Signal
import Circuit
import AmpOp

type Iteration = Int

makePlotIterations = do
    outh <- openFile "output.txt" WriteMode
    mapM_ (\(i, o) -> hPutStrLn outh (show i ++ " " ++ show o)) (ampOpInvertingList' lm741 12 r1 r2)
    hClose outh

ampOpInvertingList' :: AmpOp -> Input -> Resistor -> Resistor -> [(Iteration, Output)]
ampOpInvertingList' model input r1 r2 = fix calculate (0, 100, [], 1)
    where calculate f (vOutOld, vXOld, l, it) = if abs(vOutOld - vOut) <= 0.000001 && abs(vXOld - vX) <= 0.000001 then l ++ [(it, vOut)] else f (vOut, vX, l ++ [(it, vOut)], it + 1)
            where vX = (- vOutOld) / openLoopGain model
                  vOut = (vXOld * (r1 + r2) - (input * r2)) / r1

checkDataIterations :: IO ()
checkDataIterations = print (ampOpInvertingList' lm741 12 r1 r2)