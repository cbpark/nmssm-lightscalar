{-# LANGUAGE RecordWildCards #-}

module Analysis.NMSSM.Coupling where

import Analysis.Data         (mHSM, mS, vEW)
import Analysis.EFT.Coupling (coupling)
import Analysis.LoopFuncs    (loopFuncFermion')
import Analysis.Type

couplingH :: MixingAngles -> Double -> HiggsCoupling
couplingH (MixingAngles th1 th2 _) tanb =
    let cv = cos th1 * cos th2
        ct = cv + sin th1 / tanb
        cb = cv - sin th1 * tanb
        cTree = TreeLevelCouplings {cTop = ct, cBottom = cb, cVector = cv}
    in coupling mHSM cTree (0, 0)

couplingS :: MixingAngles -> NMSSMParameters -> HiggsCoupling
couplingS (MixingAngles th1 th2 th3) NMSSMParameters {..} =
    let cv = sin th1 * cos th2 * sin th3 + sin th2 * cos th3
        ct = cv - cos th1 * sin th3 / tanbeta
        cb = cv + cos th1 * sin th3 * tanbeta
        cTree = TreeLevelCouplings {cTop = ct, cBottom = cb, cVector = cv}
        delGam = lambda * vEW / (6 * mu) * loopFuncFermion' mS mu
                 * (cos th2 * cos th3 - sin th1 * sin th2 * sin th3)
    in coupling mS cTree (0, delGam)
