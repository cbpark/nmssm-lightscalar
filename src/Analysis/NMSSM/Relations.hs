{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

module Analysis.NMSSM.Relations
    (
      getMH3
    , getMu
    , getLambda
    , getBigLambda
    ) where

import Analysis.Data  (mHSM, mS, mZ, vEW)
import Analysis.Type
import Analysis.Util  (sincos)

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
eqF :: MixingAngles -> Double -> Double -> TanBeta -> Rel
eqF (MixingAngles th1 th2 th3) r signMu (TanBeta tanb) =
    let (!mZ2, !mS2, !mHSM2) = (massSq mZ, massSq mS, massSq mHSM)
        (sinth1, costh1) = sincos th1
        (sinth2, costh2) = sincos th2
        (sinth3, costh3) = sincos th3

        !tanbSq = tanb * tanb
        !sin2b = 2 * tanb / (1 + tanbSq)
        !cos2b = (1 - tanbSq) / (1 + tanbSq)
        !coeff = costh1 / (2 * sin2b * cos2b)

        f mH = let !mHSq = mH * mH
               in (/ (-r)) . (* signMu) $
                  mZ2 + coeff * (2 * sinth2 * sinth3 * costh3 * (mHSq - mS2)
                                 + 2 * sinth1 * costh2
                                    * (mHSM2
                                       - costh3 ** 2 * mHSq - sinth3 ** 2 * mS2))

        f' mH = (/ (-r)) . (* signMu) $
                4 * coeff * mH * costh3 * (sinth2 * sinth3
                                           - sinth1 * costh2 * costh3)
    in Rel f f'


-- |
-- From the second equation in (24) of
-- [arXiv:1211.0875](https://arxiv.org/abs/1211.0875).
eqG :: MixingAngles -> TanBeta -> Rel
eqG (MixingAngles th1 th2 th3) (TanBeta tanb) =
    let (!mS2, !mHSM2) = (massSq mS, massSq mHSM)
        (sinth1, costh1) = sincos th1
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

        g' mHSq = sum $ map ($ mHSq) [g1', g2', g3', g4']
    in Rel g g'

{-
  lambda^2 v^2 = F(mH), lambda v mu = G(mH).

  Define r = lambda v / |mu|, so lambda v = r |mu|. Then,

  r^2 mu^2 = F(mH), sign(mu) r mu^2 = G(mH).

  By combining them, - (sign(mu) / r) F(mH) + G(mH) = 0,
  which can be used to obtain mH.
-}
getMH3 :: MixingAngles
       -> Double  -- ^ r = \lambda v / |\mu|
       -> Double  -- ^ sign(\mu)
       -> TanBeta
       -> Maybe Mass
getMH3 ang r signMu tanb =
    let eq = eqF ang r signMu tanb <> eqG ang tanb
    in case newton eq 1000.0 1.0e-6 of
           Just mH3 -> if mH3 < 0  -- something went wrong!
                       then Nothing
                       else Just $ Mass mH3
           _        -> Nothing

getMu :: MixingAngles -> Double -> Double -> TanBeta -> Mass -> Mass
getMu ang r signMu tanb (Mass mH3) =
    let Rel g _ = eqG ang tanb
        !muSq =  g mH3 / r
    in Mass $ signMu * if muSq >= 0 then sqrt muSq else sqrt (- muSq)

getLambda :: Double -> Mass -> Double
getLambda r (Mass muVal) = r * abs muVal / vEW

-- |
-- From the third equation in (24) of
-- [arXiv:1211.0875](https://arxiv.org/abs/1211.0875).
getBigLambda :: MixingAngles
             -> Double  -- ^ \lambda
             -> TanBeta
             -> Mass    -- ^ m_H
             -> Mass
getBigLambda (MixingAngles (Angle th1) (Angle th2) (Angle th3)) lam (TanBeta tanb) mH =
    let (!mS2, !mHSM2, !mH2) = (massSq mS, massSq mHSM, massSq mH)
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
