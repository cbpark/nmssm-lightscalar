{-# LANGUAGE RecordWildCards #-}

module Analysis.EFT.Coupling
    (
      coupling
    , couplingSM
    , couplingHSM
    , cTotSq) where

import Analysis.Data      (BR (..), mBottom, mHSM, mTop, mW)
import Analysis.LoopFuncs (loopFuncFermion', loopFuncVector')
import Analysis.Type

cGlu :: Mass                -- ^ mass of the Higgs boson
     -> TreeLevelCouplings
     -> Double              -- ^ contributions from new particles
     -> Double
cGlu mH TreeLevelCouplings {..} delta =
    let tLoop = loopFuncFermion' mH mTop
        bLoop = loopFuncFermion' mH mBottom
    in cTop * tLoop + cBottom * bLoop + delta

cGam :: Mass                -- ^ mass of the Higgs boson
     -> TreeLevelCouplings
     -> Double              -- ^ contributions from new particles
     -> Double
cGam mH TreeLevelCouplings {..} delta =
    let tLoop = loopFuncFermion' mH mTop
        wLoop = loopFuncVector'  mH mW
    in (2 / 9) * cTop * tLoop - (7 / 8) * cVector * wLoop + delta

coupling :: Mass -> TreeLevelCouplings -> (Double, Double) -> HiggsCoupling
coupling mH cTree@TreeLevelCouplings {..} (delGlu, delGam) =
    let cg  = cGlu mH cTree delGlu
        cga = cGam mH cTree delGam
    in HiggsCoupling cTree (LoopLevelCouplings cg cga)

couplingSM :: Mass -> HiggsCoupling
couplingSM mH = coupling mH (TreeLevelCouplings 1 1 1) (0, 0)

couplingHSM :: HiggsCoupling
couplingHSM = couplingSM mHSM

cTotSq :: Mass -> HiggsCoupling -> BR -> Double
cTotSq mH (HiggsCoupling t l) BR {..} =
    let loopSM = loop (couplingSM mH)
    in bb * cBottom t ** 2 + cc * cTop t ** 2 + tautau * cBottom t ** 2
       + mumu * cBottom t ** 2
       + (ww + zz) * cVector t ** 2
       + gg * (cGluon l / cGluon loopSM) ** 2
       + gaga * (cGamma l / cGamma loopSM) ** 2
