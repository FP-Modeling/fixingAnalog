{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils where
import Control.Monad.Fix
import Control.Monad
import System.IO
import Signal
import Circuit
import AmpOp

type Iteration = Int

makePlotIterations = do
    outh <- openFile "outputIterations.txt" WriteMode
    mapM_ (\(i, o) -> hPutStrLn outh (show i ++ " " ++ show o)) (ampOpInvertingList' lm741 12 r1 r2)
    hClose outh

ampOpInvertingList' :: AmpOp -> Input -> Resistor -> Resistor -> [(Iteration, Output)]
ampOpInvertingList' model input r1 r2 = fix calculate (100, 0, [], 1)
    where calculate f (vXOld, vOutOld, l, it) = if abs(vOutOld - vOut) <= 0.000001 && abs(vXOld - vX) <= 0.000001 then l ++ [(it, vOut)] else f (vOut, vX, l ++ [(it, vOut)], it + 1)
            where vX = (- vOutOld) / openLoopGain model
                  vOut = (vXOld * (r1 + r2) - (input * r2)) / r1

checkDataIterations :: IO ()
checkDataIterations = print (ampOpInvertingList' lm741 12 r1 r2)

makePlotTime = do
    outh <- openFile "outputTime.txt" WriteMode
    mapM_ (\(t, i, o) -> hPutStrLn outh (show t ++ " " ++ show i ++ " " ++ show o)) (concat $ makeSamples outlet [1..100.0])
    hClose outh

ampOpInvertingList :: AmpOp -> Resistor -> Resistor -> Circuit Input [(Iteration, Output)]
ampOpInvertingList model r1 r2 =
    ($ (dc10000000, ground, [], 1))<$> mfix
    (\f -> Circuit $ \vIn -> Signal $ \time (vXOld, vOutOld, l, it) ->
        let vX = (/ openLoopGain model) <$> (- vOutOld)
            vOut = (/ r1) <$> (((* (r1 + r2)) <$> vXOld) - ((* r2) <$> vIn))
        in if abs(vOutOld - vOut) <= eps && abs(vXOld - vX) <= eps
            then l ++ [(it, vOut `at` time)]
            else f (vX, vOut, l ++ [(it, vOut `at` time)], it + 1))

makeSamples :: Signal Input -> [Time] -> [[(Time, Iteration, Output)]]
makeSamples input samples = do time <- samples
                               let results = (ampOpInvertingList lm741 r1 r2 `simulate` input) `at` time
                               mapM (\(i, o) -> [(time, i, o)]) results

checkDataTime :: [(Iteration, Output)]
checkDataTime = do (i, o) <- (ampOpInvertingList lm741 r1 r2 `simulate` outlet) `at` 1
                   return (i,o)

-- checkDataTime :: IO ()
-- checkDataTime = print (ampOpInvertingList lm741 r1 r2 dc12)            