{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}

module Analysis.EFT.Coupling
    (
      coupling
    , couplingSM
    , couplingHSM
    , cTotSq
    , correctSignYukawa
    ) where

import Analysis.Data      (BR (..), mBottom, mHSM, mTop, mW)
import Analysis.LoopFuncs (loopFuncFermion', loopFuncVector')
import Analysis.Type

correctSignYukawa :: HiggsCoupling -> Bool
correctSignYukawa (HiggsCoupling t _) = cTop t >= 0 && cBottom t >= 0
correctSignYukawa NullHiggsCoupling   = False

cGlu :: Mass                -- ^ mass of the Higgs boson
     -> TreeLevelCouplings
     -> Double              -- ^ contributions from new particles
     -> Double
cGlu mH TreeLevelCouplings {..} delta =
    let !tLoop = loopFuncFermion' mH mTop
        !bLoop = loopFuncFermion' mH mBottom
    in cTop * tLoop + cBottom * bLoop + delta
cGlu _ NullTreeLevelCouplings _ = 0

cGam :: Mass                -- ^ mass of the Higgs boson
     -> TreeLevelCouplings
     -> Double              -- ^ contributions from new particles
     -> Double
cGam mH TreeLevelCouplings {..} delta =
    let !tLoop = loopFuncFermion' mH mTop
        !wLoop = loopFuncVector'  mH mW
    in (2 / 9) * cTop * tLoop - (7 / 8) * cVector * wLoop + delta
cGam _ NullTreeLevelCouplings _ = 0

coupling :: Mass -> TreeLevelCouplings -> (Double, Double) -> HiggsCoupling
coupling mH cTree@TreeLevelCouplings {..} (delGlu, delGam) =
    let !cg  = cGlu mH cTree delGlu
        !cga = cGam mH cTree delGam
    in HiggsCoupling cTree (LoopLevelCouplings cg cga)
coupling _ NullTreeLevelCouplings _ =
    HiggsCoupling NullTreeLevelCouplings NullLoopLevelCouplings

couplingSM :: Mass -> HiggsCoupling
couplingSM mH = coupling mH (TreeLevelCouplings 1 1 1) (0, 0)

couplingHSM :: HiggsCoupling
couplingHSM = couplingSM mHSM

cTotSq :: Mass -> BR -> HiggsCoupling -> Double
cTotSq mH BR {..} (HiggsCoupling t l) =
    let !cSM = couplingSM mH
        !loopSM = loop cSM
    in bb * cBottom t ** 2 + cc * cTop t ** 2 + tautau * cBottom t ** 2
       + mumu * cBottom t ** 2
       + (ww + zz) * cVector t ** 2
       + gg * (cGluon l / cGluon loopSM) ** 2
       + gaga * (cGamma l / cGamma loopSM) ** 2
cTotSq _ _ NullHiggsCoupling = 1.0e+12
