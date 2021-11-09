{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils where
import Lucid
import Lucid.Html5
import Graphics.Plotly
import Graphics.Plotly.Lucid
import Lens.Micro
import Control.Monad.Fix
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Signal
import Circuit
import AmpOp

type Iteration = Int

makePlot =
    T.writeFile "output.html" $ renderText $ doctypehtml_ $ do
    head_ $ do meta_ [charset_ "utf-8"]
               plotlyCDN
    body_ $ toHtml $ plotly "myDiv" [myTrace] 

myTrace
  = line (aes & x .~ fst
              & y .~ snd) (ampOpInvertingList' lm741 12 r1 r2)

ampOpInvertingList' :: AmpOp -> Input -> Resistor -> Resistor -> [(Iteration, Output)]
ampOpInvertingList' model input r1 r2 = fix calculate (0, 100, [], 0)
    where calculate f (vOutOld, vXOld, l, it) = if abs(vOutOld - vOut) <= 0.000001 && abs(vXOld - vX) <= 0.000001 then l ++ [(it, vOut)] else f (vOut, vX, l ++ [(it, vOut)], it + 1)
            where vX = (- vOutOld) / openLoopGain model
                  vOut = (vXOld * (r1 + r2) - (input * r2)) / r1

checkData :: IO ()
checkData = print (ampOpInvertingList' lm741 12 r1 r2)

--writeGraphFile :: IO ()
--writeGraphFile = Bin.encodeFile "output.txt" $ ampOpInvertingList' lm741 12 r1 r2