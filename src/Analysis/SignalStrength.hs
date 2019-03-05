{-# LANGUAGE RecordWildCards #-}

module Analysis.SignalStrength where

import Analysis.Data
import Analysis.EFT.Coupling (cTotSq, couplingHSM, couplingSM)
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
  where ctot2 = cTotSq mHSM brHSM c

muVV, muBB, muTauTau, muGaGa :: HiggsCoupling -> Double
muVV     = muH (\c -> cVector (tree c) ** 2)
muBB     = muH (\c -> cBottom (tree c) ** 2)
muTauTau = muBB
muGaGa   = muH (\c -> (cGamma (loop c) / cGamma (loop couplingHSM)) ** 2)

satisfyMu :: MuData -> Double -> Bool
satisfyMu MuData {..} mu = mu >= (central + lower) && mu <= (central + upper)

satisfyMuWW13, satisfyMuZZ13, satisfyMuBB13, satisfyMuTauTau13, satisfyMuGaGa13
    :: Double -> Bool
satisfyMuWW13     = satisfyMu muWW13
satisfyMuZZ13     = satisfyMu muZZ13
satisfyMuBB13     = satisfyMu muBB13
satisfyMuTauTau13 = satisfyMu muTauTau13
satisfyMuGaGa13   = satisfyMu muGaGa13

ctot2S :: HiggsCoupling -> Double
ctot2S = cTotSq mS brSSM

muCMS :: HiggsCoupling -> Double
muCMS c@(HiggsCoupling _ l) = (cglu * cgam ) ** 2 / ctot2S c
  where
    (HiggsCoupling _ l0) = couplingSM mS
    cglu = cGluon l / cGluon l0
    cgam = cGamma l / cGamma l0

muLEP :: HiggsCoupling -> Double
muLEP c@(HiggsCoupling t _) = (cvec * cb) ** 2 / ctot2S c
  where
    cvec = cVector t
    cb   = cBottom t
