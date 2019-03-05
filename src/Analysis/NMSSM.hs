module Analysis.NMSSM where

import Analysis.EFT.SignalStrength (muCMS, muLEP)
import Analysis.NMSSM.Coupling     (couplingS)
import Analysis.Type

getMu :: (HiggsCoupling -> Double) -> MixingAngles -> NMSSMParameters -> Double
getMu muF ths params = muF (couplingS ths params)

getMuCMS, getMuLEP :: MixingAngles -> NMSSMParameters -> Double
getMuCMS = getMu muCMS
getMuLEP = getMu muLEP
