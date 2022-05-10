{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils where
import Control.Monad.Fix
import Control.Monad
import System.IO
import Signal
import Circuit
import AmpOp
    ( SignalState(SignalTwoNodes),
      AmpOp(openLoopGain),
      Impedance,
      Output,
      Input,
      dc100,
      sen,
      ground,
      lm741,
      r1,
      r2,
      eps )
import Data.List
import Data.Complex
import Simulation

type Iteration = Int

makePlotIterations = do
    outh <- openFile "outputIterations.txt" WriteMode
    mapM_ (\(i, o) -> hPutStrLn outh (show i ++ " " ++ show o)) (ampOpInvertingList' lm741 12 r1 r2)
    hClose outh

ampOpInvertingList' :: AmpOp -> Input -> Impedance -> Impedance -> [(Iteration, Output)]
ampOpInvertingList' model input r1 r2 = fix calculate (100, 0, [], 1)
    where calculate f (vXOld, vOutOld, l, it) = if abs(realPart vOutOld - realPart vOut) <= 0.000001 && abs(realPart vXOld - realPart vX) <= 0.000001 then l ++ [(it, vOut)] else f (vOut, vX, l ++ [(it, vOut)], it + 1)
            where vX = (- vOutOld) / openLoopGain model `at` 0
                  vOut = (vXOld * (r1 + r2) `at` 0 - input * r2 `at` 0) / r1 `at` 0

checkDataIterations :: IO ()
checkDataIterations = print (ampOpInvertingList' lm741 12 r1 r2)


ampOpInvertingList :: AmpOp -> Impedance -> Impedance -> SignalState -> Circuit Input [(Iteration, SignalState)]
ampOpInvertingList model r1 r2 (SignalTwoNodes (vXInit, vOutInit)) =
    ($ (vXInit, vOutInit, [], 1))<$> mfix
    (\f -> Circuit $ \vIn -> Signal $ \time (vXOld, vOutOld, l, it) ->
        let vX = (- vOutOld) / openLoopGain model
            vOut = ((vXOld * (r1 + r2)) - (vIn * r2)) / r1
        in if abs((realPart <$> vOutOld) - (realPart <$> vOut)) <= eps && abs((realPart <$> vXOld) - (realPart <$> vX)) <= eps
            then l ++ [(it, SignalTwoNodes (vX, vOut))]
            else f (vX, vOut, l ++ [(it, SignalTwoNodes (vX, vOut))], it + 1))

makeSamplesInput :: Signal Input -> [Metric] -> [(Metric, Input)]
makeSamplesInput input samples = do time <- samples
                                    let result = input `at` time
                                    return (time, result)

makeSamplesOutput :: SignalState -> (SignalState -> Circuit Input [(Iteration, SignalState)]) -> Signal Input -> [Metric] -> [[(Metric, Iteration, Output)]]
makeSamplesOutput _ _ _ [] = []
makeSamplesOutput initialState circuit input (t:ts) = map (\(i, SignalTwoNodes (_, o)) -> (t, i, o `at` t)) results : rest
    where results = circuit initialState `simulate` input `at` t
          rest = makeSamplesOutput (snd $ last results) circuit input ts

ampOpInvertingList2 :: AmpOp -> Impedance -> Impedance -> Circuit Input [(Iteration, Output)]
ampOpInvertingList2 model r1 r2 =
    ($ (dc100, ground, [], 1))<$> mfix
    (\f -> Circuit $ \vIn -> Signal $ \time (vXOld, vOutOld, l, it) ->
        let vX = (- vOutOld) / openLoopGain model
            vOut = ((vXOld * (r1 + r2)) - (vIn * r2)) / r1
        in if abs((realPart <$> vOutOld) - (realPart <$> vOut)) <= eps && abs((realPart <$> vXOld) - (realPart <$> vX)) <= eps
            then l ++ [(it, vOut `at` time)]
            else f (vX, vOut, l ++ [(it, vOut `at` time)], it + 1))

makeSamplesOutput2 :: Signal Input -> [Metric] -> [[(Metric, Iteration, Output)]]
makeSamplesOutput2 input samples = do time <- samples
                                      let results = (ampOpInvertingList2 lm741 r1 r2 `simulate` input) `at` time
                                      mapM (\(i, o) -> [(time, i, o)]) results

testCircuit = ampOpInvertingList lm741 r1 r2

makePlotMetricOutput = do
    outh <- openFile "outputMetric.txt" WriteMode
    mapM_ (\(t, i, o) -> hPutStrLn outh (show t ++ " " ++ show i ++ " " ++ show o)) (concat $ makeSamplesOutput iSignalState testCircuit sen (map (:+ 0) [0.1,0.2..10.0]))
    hClose outh

makePlotMetricInput = do
    outh <- openFile "inputMetric.txt" WriteMode
    mapM_ (\(t, o) -> hPutStrLn outh (show t ++ " " ++ show o)) (makeSamplesInput sen (map (:+ 0) [0.1,0.2..10.0]))
    hClose outh                                   
