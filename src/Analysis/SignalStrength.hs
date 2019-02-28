module Analysis.SignalStrength where

import Analysis.Data         (XSec (..), brHSM, mHSM, xsecHSM13)
import Analysis.EFT.Coupling (cTotSq, couplingHSM)
import Analysis.Type

xsecRatioHSM :: HiggsCoupling -> Double
xsecRatioHSM (HiggsCoupling t l) = num / den
  where
    num = (cGluon l / cGluon (loop couplingHSM)) ** 2 * ggf xsecHSM13
          + cVector t ** 2 * (vbf xsecHSM13 + hv xsecHSM13)
          + cTop t ** 2 * htt xsecHSM13
    den = sum $ map ($ xsecHSM13) [ggf, vbf, hv, htt]

muSM :: (HiggsCoupling -> Double) -> HiggsCoupling -> Double
muSM getCSq c = ((*) <$> xsecRatioHSM <*> getCSq) c / ctot2
  where ctot2 = cTotSq mHSM c brHSM

muVVSM, mubbSM, muTauTauSM, muGaGaSM :: HiggsCoupling -> Double
muVVSM     = muSM (\c -> cVector (tree c) ** 2)
mubbSM     = muSM (\c -> cBottom (tree c) ** 2)
muTauTauSM = mubbSM
muGaGaSM   = muSM (\c -> (cGamma (loop c) / cGamma (loop couplingHSM)) ** 2)
