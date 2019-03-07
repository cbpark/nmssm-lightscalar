module Analysis.Data where

import Analysis.Type (Mass)

mTop, mBottom, mW, mZ :: Mass
mTop    = 173.0
mBottom = 4.7
mW      = 80.379
mZ      = 91.1876

mHSM, mS, mH :: Mass
mHSM = 125.0
mS   = 96.0
mH   = 1000.0

vEW :: Double
vEW = 174.10358473791823

data BR = BR { bb     :: Double
             , cc     :: Double
             , tautau :: Double
             , mumu   :: Double
             , ww     :: Double
             , zz     :: Double
             , gg     :: Double
             , gaga   :: Double
             } deriving Show

brHSM0, brSSM0 :: BR
-- |
-- SM Higgs Branching Ratios and Total Decay Widths (update in CERN Report4 2016)
--
-- https://twiki.cern.ch/twiki/bin/view/LHCPhysics/CERNYellowReportPageBR
brHSM0 = BR { bb     = 5.809e-1
            , cc     = 2.891e-2
            , tautau = 6.272e-2
            , mumu   = 2.176e-4
            , ww     = 2.137e-1
            , zz     = 2.619e-2
            , gg     = 8.187e-2
            , gaga   = 2.270e-3
            }
-- | Using HDECAY with setting mH = 96 GeV.
brSSM0 = BR { bb     = 8.050e-1
            , cc     = 3.994e-2
            , tautau = 8.217e-2
            , mumu   = 2.854e-4
            , ww     = 5.580e-3
            , zz     = 7.305e-4
            , gg     = 6.456e-2
            , gaga   = 1.412e-3
            }

rescaleBR :: BR -> BR
rescaleBR br =
    let s = sum $ map ($ br) [bb, cc, tautau, mumu, ww, zz, gg, gaga]
    in BR { bb     = bb     br / s
          , cc     = cc     br / s
          , tautau = tautau br / s
          , mumu   = mumu   br / s
          , ww     = ww     br / s
          , zz     = zz     br / s
          , gg     = gg     br / s
          , gaga   = gaga   br / s
          }

brHSM, brSSM :: BR
brHSM = rescaleBR brHSM0
brSSM = rescaleBR brSSM0

data XSec = XSec { ggf :: Double
                 , vbf :: Double
                 , hv  :: Double
                 , htt :: Double
                 } deriving Show

-- | From SM Higgs production cross sections at sqrt(s) = 13 TeV
-- (update in CERN Report4 2016)
--
-- https://twiki.cern.ch/twiki/bin/view/LHCPhysics/CERNYellowReportPageAt13TeV
xsecHSM13 :: XSec
xsecHSM13 = XSec { ggf = 45.2, vbf = 3.925, hv = 2.3845, htt = 0.4987 }

data MuData = MuData { central :: Double
                     , lower   :: Double
                     , upper   :: Double
                     } deriving Show

-- | From Table I of
-- [arXiv:1810.02521](https://arxiv.org/abs/1810.02521).
muWW13, muZZ13, muBB13, muTauTau13, muGaGa13 :: MuData
muWW13     = MuData 1.20 (-0.13) 0.14
muZZ13     = MuData 1.05 (-0.11) 0.11
muBB13     = MuData 1.05 (-0.19) 0.19
muTauTau13 = MuData 1.15 (-0.23) 0.24
muGaGa13   = MuData 1.10 (-0.10) 0.10

muCMSData, muLEPData :: MuData
muCMSData = MuData 0.6   (-0.2)   0.2
muLEPData = MuData 0.117 (-0.057) 0.057
