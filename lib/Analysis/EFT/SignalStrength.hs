{-# LANGUAGE RecordWildCards #-}

module Analysis.EFT.SignalStrength where

import Analysis.Data
import Analysis.EFT.Coupling (cTotSq, couplingHSM, couplingSM)
import Analysis.Type

xsecRatioHSM :: HiggsCoupling -> Double
xsecRatioHSM NullHiggsCoupling   = 0
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

satisfyMu :: MuData -> Double -> Double -> Bool
satisfyMu MuData {..} nsigma mu =
    mu >= (central + nsigma * lower) && mu <= (central + nsigma * upper)

satisfyMuWW13, satisfyMuZZ13, satisfyMuBB13, satisfyMuTauTau13, satisfyMuGaGa13
    :: Double -> Double -> Bool
satisfyMuWW13     = satisfyMu muWW13
satisfyMuZZ13     = satisfyMu muZZ13
satisfyMuBB13     = satisfyMu muBB13
satisfyMuTauTau13 = satisfyMu muTauTau13
satisfyMuGaGa13   = satisfyMu muGaGa13

satisfyMuCMS, satisfyMuLEP :: Double -> Double -> Bool
satisfyMuCMS = satisfyMu muCMSData
satisfyMuLEP = satisfyMu muLEPData

ctot2S :: HiggsCoupling -> Double
ctot2S = cTotSq mS brSSM

muCMS :: HiggsCoupling -> Double
muCMS NullHiggsCoupling     = 0
muCMS c@(HiggsCoupling _ l) = (cglu * cgam ) ** 2 / ctot2S c
  where
    (HiggsCoupling _ l0) = couplingSM mS
    cglu = cGluon l / cGluon l0
    cgam = cGamma l / cGamma l0

muLEP :: HiggsCoupling -> Double
muLEP NullHiggsCoupling     = 0
muLEP c@(HiggsCoupling t _) = (cv * cb) ** 2 / ctot2S c
  where
    cv = cVector t
    cb = cBottom t

muZZGGF :: HiggsCoupling -> Double
muZZGGF    NullHiggsCoupling  = 0
muZZGGF c@(HiggsCoupling t l) =
    (cGluon l / cGluon (loop couplingHSM)) ** 2 * cVector t ** 2 / ctot2
  where ctot2 = cTotSq mHSM brHSM c

satisfyMuZZ :: Double -> Double -> Bool
satisfyMuZZ = satisfyMu muZZATLAS

brRatioGaGa, brRatioTauTau, brRatioBB :: HiggsCoupling -> Double
brRatioGaGa NullHiggsCoupling   = 0
brRatioGaGa (HiggsCoupling t l) =
    (cGamma l / cGamma (loop couplingHSM)) ** 2 / cVector t ** 2
brRatioTauTau NullHiggsCoupling = 0
brRatioTauTau (HiggsCoupling t _) = (cBottom t / cVector t) ** 2
brRatioBB = brRatioTauTau

satisfyBrRatioGaGa, satisfyBrRatioTauTau, satisfyBrRatioBB
    :: Double -> Double -> Bool
satisfyBrRatioGaGa   = satisfyMu brRatioGaGaATLAS
satisfyBrRatioTauTau = satisfyMu brRatioTauTauATLAS
satisfyBrRatioBB     = satisfyMu brRatioBBATLAS
