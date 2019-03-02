{-# LANGUAGE RecordWildCards #-}

module Analysis.SignalStrength where

import Analysis.Data
import Analysis.EFT.Coupling (cTotSq, couplingHSM)
import Analysis.Type

xsecRatioHSM :: HiggsCoupling -> Double
xsecRatioHSM (HiggsCoupling t l) = num / den
  where
    num = (cGluon l / cGluon (loop couplingHSM)) ** 2 * ggf xsecHSM13
          + cVector t ** 2 * (vbf xsecHSM13 + hv xsecHSM13)
          + cTop t ** 2 * htt xsecHSM13
    den = sum $ map ($ xsecHSM13) [ggf, vbf, hv, htt]

muH :: (HiggsCoupling -> Double) -> HiggsCoupling -> Double
muH getCSq c = ((*) <$> xsecRatioHSM <*> getCSq) c / ctot2
  where ctot2 = cTotSq mHSM c brHSM

muVVH, muBBH, muTauTauH, muGaGaH :: HiggsCoupling -> Double
muVVH     = muH (\c -> cVector (tree c) ** 2)
muBBH     = muH (\c -> cBottom (tree c) ** 2)
muTauTauH = muBBH
muGaGaH   = muH (\c -> (cGamma (loop c) / cGamma (loop couplingHSM)) ** 2)

satisfyMu :: MuData -> Double -> Bool
satisfyMu MuData {..} mu = mu >= (central + lower) && mu <= (central + upper)

satisfyMuWW13, satisfyMuZZ13, satisfyMuBB13, satisfyMuTauTau13, satisfyMuGaGa13
    :: Double -> Bool
satisfyMuWW13     = satisfyMu muWW13
satisfyMuZZ13     = satisfyMu muZZ13
satisfyMuBB13     = satisfyMu muBB13
satisfyMuTauTau13 = satisfyMu muTauTau13
satisfyMuGaGa13   = satisfyMu muGaGa13
