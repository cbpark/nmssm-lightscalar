{-# LANGUAGE RecordWildCards #-}

module Analysis.EFT.Coupling (couplings, couplingsSM) where

import Analysis.Data      (mBottom, mTop, mW)
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

couplings :: Mass -> TreeLevelCouplings -> (Double, Double) -> HiggsCoupling
couplings mH cTree@TreeLevelCouplings {..} (delGlu, delGam) =
    let cg  = cGlu mH cTree delGlu
        cga = cGam mH cTree delGam
    in HiggsCoupling cTree (LoopLevelCouplings cg cga)

couplingsSM :: Mass -> HiggsCoupling
couplingsSM mH = couplings mH (TreeLevelCouplings 1 1 1) (0, 0)
