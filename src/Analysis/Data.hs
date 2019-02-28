module Analysis.Data where

import Analysis.Type (Mass)

mTop, mBottom, mW, mZ :: Mass
mTop    = 173.0
mBottom = 4.7
mW      = 80.379
mZ      = 91.1876

mHSM :: Mass
mHSM = 125.0

mS :: Mass
mS = 96.0

vEW :: Double
vEW = 174.10358473791823

data BR = BR { bb     :: Double
             , cc     :: Double
             , tautau :: Double
             , ww     :: Double
             , zz     :: Double
             , gg     :: Double
             , gaga   :: Double
             } deriving Show

brHSM :: BR
brHSM = BR { bb     = 5.809e-1
           , cc     = 2.891e-2
           , tautau = 6.272e-2
           , ww     = 2.137e-1
           , zz     = 2.619e-1
           , gg     = 8.187e-2
           , gaga   = 2.270e-3 }

brSSM :: BR
brSSM = BR { bb     = 8.050e-1
           , cc     = 3.994e-2
           , tautau = 8.217e-2
           , ww     = 0.5580e-2
           , zz     = 0.7305e-3
           , gg     = 6.456e-2
           , gaga   = 1.412e-3 }

data XSec = XSec { ggf :: Double
                 , vbf :: Double
                 , hv  :: Double
                 , htt :: Double } deriving Show

xsecHSM13 :: XSec
xsecHSM13 = XSec { ggf = 45.2, vbf = 3.925, hv = 2.3845, htt = 0.4987 }
