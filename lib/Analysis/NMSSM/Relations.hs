{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

module Analysis.NMSSM.Relations
    (
      getMH3M0
    , getMu
    , getLambda
    , getBigLambda
    ) where

import Analysis.Data  (mHSM2, mS2, mZ2, vEW, vEW2)
import Analysis.Type
import Analysis.Util  (sincos)

import Control.Monad  (guard)
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup (..))
#endif

data Rel = Rel { rel :: Double -> Double, diffrel :: Double -> Double }

instance Semigroup Rel where
    Rel rel1 diffrel1 <> Rel rel2 diffrel2 =
        let sumrel     x = rel1     x + rel2     x
            sumdiffrel x = diffrel1 x + diffrel2 x
        in Rel sumrel sumdiffrel

-- |
-- From the first equation in (24) of
-- [arXiv:1211.0875](https://arxiv.org/abs/1211.0875).
eqF :: MixingAngles -> Double -> Double -> TanBeta -> Epsilon
    -> Rel  -- ^ equations for m_0(m_H)
    -> Rel
eqF (MixingAngles th1 th2 th3) r signMu (TanBeta tanb)
    (Epsilon eps) (Rel m0Sq m0Sq') =
    let (sinth1, costh1) = sincos th1
        (sinth2, costh2) = sincos th2
        (sinth3, costh3) = sincos th3

        !tanbSq = tanb * tanb
        !sin2b = 2 * tanb / (1 + tanbSq)
        !cos2b = (1 - tanbSq) / (1 + tanbSq)
        !sin4b = 2 * sin2b * cos2b
        !coeff = costh1 / sin4b

        -- \\Delta m_{12}^2 = - (m_0^2 - m_Z^2) / tan(beta) + \\epsilon v^2,
        -- where m_0^2 is a function of m_H.
        delM12  mH = - (m0Sq mH - mZ2) / tanb + eps * vEW2
        delM12' mH = - m0Sq' mH / tanb
        !coeffM12 = 2 / sin4b

        f mH = let !mHSq = mH * mH
               in (/ (-r)) . (* signMu) $
                  mZ2 + coeff * (2 * sinth2 * sinth3 * costh3 * (mHSq - mS2)
                                 + 2 * sinth1 * costh2
                                    * (mHSM2
                                       - costh3 ** 2 * mHSq - sinth3 ** 2 * mS2))
                  + coeffM12 * delM12 mH  -- (2 / sin(4 beta)) \\Delta m_{12}^2

        f' mH = (/ (-r)) . (* signMu) $
                4 * coeff * mH * costh3 * (sinth2 * sinth3
                                           - sinth1 * costh2 * costh3)
                + coeffM12 * delM12' mH
    in Rel f f'

-- |
-- From the second equation in (24) of
-- [arXiv:1211.0875](https://arxiv.org/abs/1211.0875).
eqG :: MixingAngles -> TanBeta -> Rel
eqG (MixingAngles th1 th2 th3) (TanBeta tanb) =
    let (sinth1, costh1) = sincos th1
        (sinth2, costh2) = sincos th2
        (sinth3, costh3) = sincos th3

        !tan2b = 2 * tanb / (1 - tanb * tanb)

        g1 _  = - 0.5 * mHSM2 * costh1 ** 2 * sinth2 * costh2
        g2 mH = - 0.5 * (mH * mH - mS2)
                * sinth1 * (costh2 ** 2 - sinth2 ** 2) * sinth3 * costh3
        g3 mH = 0.5 * ((mH * mH - mS2 * sinth1 ** 2) * sinth3 ** 2
                       - (mH * mH * sinth1 ** 2 - mS2) * costh3 ** 2)
                  * sinth2 * costh2
        g4 mH = - 0.25 * tan2b
                * ((mH * mH - mS2) * costh2 * 2 * sinth3 * costh3
                   - 2 * (mHSM2 - mH * mH * costh3 ** 2
                          - mS2 * sinth3 ** 2) * sinth1 * sinth2) * costh1
        g mH = sum $ map ($ mH) [g1, g2, g3, g4]

        g1' _  = 0
        g2' mH = - mH * sinth1 * (costh2 ** 2 - sinth2 ** 2) * sinth3 * costh3
        g3' mH = mH * (sinth3 ** 2 - sinth1 ** 2 * costh3 ** 2) * sinth2 * costh2
        g4' mH = - tan2b * mH
                 * (costh2 * sinth3 + costh3 * sinth1 * sinth2) * costh1 * costh3

        g' mH = sum $ map ($ mH) [g1', g2', g3', g4']
    in Rel g g'

{-
  lambda^2 v^2 = F(mH), lambda v mu = G(mH).

  Define r = lambda v / |mu|, so lambda v = r |mu|. Then,

  r^2 mu^2 = F(mH), sign(mu) r mu^2 = G(mH).

  By combining them, - (sign(mu) / r) F(mH) + G(mH) = 0,
  which can be used to obtain mH.
-}
getMH3M0 :: MixingAngles
         -> Double              -- ^ r = \lambda v / |\mu|
         -> Double              -- ^ sign(\mu)
         -> TanBeta
         -> Epsilon
         -> Maybe (Mass, Mass)  -- ^ (mH3, m0)
getMH3M0 ang r signMu tanb eps = do
    let m0F@(Rel m0SqF _) = eqM0Sq ang tanb eps
        eq = eqF ang r signMu tanb eps m0F <> eqG ang tanb

    mH3 <- newton eq 1000.0 1.0e-6
    guard $ mH3 > 0  -- something went wrong!

    let m02 = m0SqF mH3
    guard $ m02 > 0  -- why?

    return (Mass mH3, Mass (sqrt m02))

-- | From Eq. (8) of [arXiv:1407.0955](https://arxiv.org/abs/1407.0955).
eqM0Sq :: MixingAngles -> TanBeta -> Epsilon -> Rel
eqM0Sq ang (TanBeta tanb) (Epsilon eps) =
    let !tan2b = 2 * tanb / (1 - tanb * tanb)

        !oHhVal = oHh ang
        !oHHVal = oHH ang
        !oshVal = osh ang
        !osHVal = osH ang
        !fac = 1.0 / (1.0 - tan2b / tanb)

        -- m_0^2(m_H) = (1 - tan(2 beta) / tan(beta))^{-1}
        --              [f(m_H) - (m_Z^2 / tan(beta) + \\epsilon v^2) tan(2 beta)],
        --
        -- d m_0^2(m_H) / d m_H =
        --              (1 - tan(2 beta) / tan(beta))^{-1} f'(m_H).
        m0Sq mH =
            let !mH2 = mH * mH
            in (fac *) $ mHSM2
               + (mH2 - mHSM2) * oHhVal * (oHhVal + oHHVal * tan2b)
               - (mHSM2 - mS2) * oshVal * (oshVal + osHVal * tan2b)
               - (mZ2 / tanb + eps * vEW2) * tan2b

        m0Sq' mH = (fac *) $ 2 * mH * oHhVal * (oHhVal + oHHVal * tan2b)
    in Rel m0Sq m0Sq'

getMu :: MixingAngles -> Double -> Double -> TanBeta -> Mass -> Mass
getMu ang r signMu tanb (Mass mH3) =
    let Rel g _ = eqG ang tanb
        !muSq =  g mH3 / r
    in Mass $ signMu * if muSq >= 0 then sqrt muSq else sqrt (- muSq)

getLambda :: Double -> Mass -> Lambda
getLambda r (Mass muVal) = Lambda $ r * abs muVal / vEW

-- |
-- From the third equation in (24) of
-- [arXiv:1211.0875](https://arxiv.org/abs/1211.0875).
getBigLambda :: MixingAngles
             -> Lambda
             -> TanBeta
             -> Mass    -- ^ m_H
             -> Mass
getBigLambda (MixingAngles (Angle th1) (Angle th2) (Angle th3)) (Lambda lam) (TanBeta tanb) mH =
    let mH2 = massSq mH
        !tanbSq = tanb * tanb
        !cos2b = (1 - tanbSq) / (1 + tanbSq)

        rhs = - 0.5 / cos2b
              * ((mH2 - mS2) * cos th2 * 2 * sin th3 * cos th3
                 - 2 * (mHSM2 - mH2 * cos th3 ** 2 - mS2 * sin th3 ** 2)
                 * sin th1 * sin th2) * cos th1
    in Mass $ rhs / (lam * vEW)

newton :: Rel
       -> Double  -- ^ initial guess
       -> Double  -- ^ tolerance
       -> Maybe Double
newton Rel {..} guess epsilon = newton' 100 guess
  where
    newton' :: Int -> Double -> Maybe Double
    newton' i guess0
        | i == 0    = Nothing
        | otherwise = let guess1 = guess0 - (rel guess0 / diffrel guess0)
                          err =  abs (guess1 - guess0)
                      in if err < epsilon
                         then Just guess1
                         else newton' (i - 1) guess1

oHh, oHH, osh, osH :: MixingAngles -> Double
oHh (MixingAngles (Angle th1) (Angle th2) (Angle th3)) =
    cos th2 * cos th3 * sin th1 - sin th2 * sin th3
oHH (MixingAngles (Angle th1) _           (Angle th3)) =
    cos th1 * cos th3
osh (MixingAngles (Angle th1) (Angle th2) (Angle th3)) =
    cos th3 * sin th2 + cos th2 * sin th1 * sin th3
osH = oHH
