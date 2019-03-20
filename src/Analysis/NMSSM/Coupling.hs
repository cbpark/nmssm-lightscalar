{-# LANGUAGE BangPatterns #-}

module Analysis.NMSSM.Coupling where

import Analysis.Data         (mHSM, mS)
import Analysis.EFT.Coupling (coupling)
import Analysis.Type

couplingH :: MixingAngles -> TanBeta -> Double -> HiggsCoupling
couplingH (MixingAngles (Angle th1) (Angle th2) _) (TanBeta tanb) r =
    let !cv = cos th1 * cos th2
        !ct = cv + sin th1 / tanb
        !cb = cv - sin th1 * tanb
        cTree = TreeLevelCouplings {cTop = ct, cBottom = cb, cVector = cv}

        !delGam = - r / 6 * cos th1 * sin th2
    in coupling mHSM cTree (0, delGam)

couplingS :: MixingAngles -> TanBeta -> Double -> HiggsCoupling
couplingS (MixingAngles (Angle th1) (Angle th2) (Angle th3)) (TanBeta tanb) r =
    let !costh1 = cos th1; !sinth1 = sin th1
        !costh2 = cos th2; !sinth2 = sin th2
        !costh3 = cos th3; !sinth3 = sin th3

        !cv = sinth1 * costh2 * sinth3 + sinth2 * costh3
        !ct = cv - costh1 * sinth3 / tanb
        !cb = cv + costh1 * sinth3 * tanb
        cTree = TreeLevelCouplings {cTop = ct, cBottom = cb, cVector = cv}

        !delGam = r / 6 * (costh2 * costh3 - sinth1 * sinth2 * sinth3)
    in coupling mS cTree (0, delGam)
