{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils where
import Control.Monad.Fix
import Control.Monad
import System.IO
import Signal
import Circuit
import AmpOp
import Data.List
import Simulation

type Iteration = Int

makePlotIterations = do
    outh <- openFile "outputIterations.txt" WriteMode
    mapM_ (\(i, o) -> hPutStrLn outh (show i ++ " " ++ show o)) (ampOpInvertingList' lm741 12 r1 r2)
    hClose outh

ampOpInvertingList' :: AmpOp -> Input -> Resistor -> Resistor -> [(Iteration, Output)]
ampOpInvertingList' model input r1 r2 = fix calculate (100, 0, [], 1)
    where calculate f (vXOld, vOutOld, l, it) = if abs(vOutOld - vOut) <= 0.000001 && abs(vXOld - vX) <= 0.000001 then l ++ [(it, vOut)] else f (vOut, vX, l ++ [(it, vOut)], it + 1)
            where vX = (- vOutOld) / openLoopGain model
                  vOut = (vXOld * (r1 + r2) - input * r2) / r1

checkDataIterations :: IO ()
checkDataIterations = print (ampOpInvertingList' lm741 12 r1 r2)


ampOpInvertingList :: AmpOp -> Resistor -> Resistor -> SignalState -> Circuit Input [(Iteration, SignalState)]
ampOpInvertingList model r1 r2 (SignalState2 (vXInit, vOutInit)) =
    ($ (vXInit, vOutInit, [], 1))<$> mfix
    (\f -> Circuit $ \vIn -> Signal $ \time (vXOld, vOutOld, l, it) ->
        let vX = (/ openLoopGain model) <$> (- vOutOld)
            vOut = (/ r1) <$> ((* (r1 + r2)) <$> vXOld) - ((* r2) <$> vIn)
        in if abs(vOutOld - vOut) <= eps && abs(vXOld - vX) <= eps
            then l ++ [(it, SignalState2 (vX, vOut))]
            else f (vX, vOut, l ++ [(it, SignalState2 (vX, vOut))], it + 1))

makeSamplesInput :: Signal Input -> [Time] -> [(Time, Input)]
makeSamplesInput input samples = do time <- samples
                                    let result = input `at` time
                                    return (time, result)

makeSamplesOutput :: SignalState -> (SignalState -> Circuit Input [(Iteration, SignalState)]) -> Signal Input -> [Time] -> [[(Time, Iteration, Output)]]
makeSamplesOutput _ _ _ [] = []
makeSamplesOutput initialState circuit input (t:ts) = map (\(i, SignalState2 (_, o)) -> (t, i, o `at` t)) results : rest
    where results = circuit initialState `simulate` input `at` t
          rest = makeSamplesOutput (snd $ last results) circuit input ts


ampOpInvertingList2 :: AmpOp -> Resistor -> Resistor -> Circuit Input [(Iteration, Output)]
ampOpInvertingList2 model r1 r2 =
    ($ (dc100, ground, [], 1))<$> mfix
    (\f -> Circuit $ \vIn -> Signal $ \time (vXOld, vOutOld, l, it) ->
        let vX = (/ openLoopGain model) <$> (- vOutOld)
            vOut = (/ r1) <$> ((* (r1 + r2)) <$> vXOld) - ((* r2) <$> vIn)
        in if abs(vOutOld - vOut) <= eps && abs(vXOld - vX) <= eps
            then l ++ [(it, vOut `at` time)]
            else f (vX, vOut, l ++ [(it, vOut `at` time)], it + 1))

makeSamplesOutput2 :: Signal Input -> [Time] -> [[(Time, Iteration, Output)]]
makeSamplesOutput2 input samples = do time <- samples
                                      let results = (ampOpInvertingList2 lm741 r1 r2 `simulate` input) `at` time
                                      mapM (\(i, o) -> [(time, i, o)]) results

testCircuit = ampOpInvertingList lm741 r1 r2

makePlotTimeOutput = do
    outh <- openFile "outputTime.txt" WriteMode
    mapM_ (\(t, i, o) -> hPutStrLn outh (show t ++ " " ++ show i ++ " " ++ show o)) (concat $ makeSamplesOutput iSignalState testCircuit sen [0.1,0.2..10.0])
    hClose outh

makePlotTimeInput = do
    outh <- openFile "inputTime.txt" WriteMode
    mapM_ (\(t, o) -> hPutStrLn outh (show t ++ " " ++ show o)) (makeSamplesInput sen [0.1,0.2..10.0])
    hClose outh                                   