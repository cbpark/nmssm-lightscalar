{-# LANGUAGE BangPatterns #-}

module Analysis.NMSSM.Relations
    (
      getTheta3
    , getMu
    , getLambda
    ) where

import Analysis.Data (mH, mHSM, mS, mZ, vEW)
import Analysis.Type
import Analysis.Util (mpiHalf2piHalf)

-- |
-- The first equation in (24) of
-- [arXiv:1211.0875](https://arxiv.org/abs/1211.0875).
getTheta3 :: (Angle, Angle)  -- ^ (theta1, theta2)
          -> Double          -- ^ lambda
          -> Double          -- ^ tan(beta)
          -> Maybe Double
getTheta3 (th1, th2) lam tanb =
    let (mZ2, mS2, mHSM2, mH2) = (mZ * mZ, mS * mS, mHSM * mHSM, mH * mH)
        lam2 = lam * lam
        v2 = vEW * vEW

        (sinth1, costh1) = sincos th1
        (sinth2, costh2) = sincos th2

        !tanbSq = tanb * tanb
        !sin2b = 2 * tanb / (1 + tanbSq)
        !cos2b = (1 - tanbSq) / (1 + tanbSq)
        !coeff =  costh1 / (2 * sin2b * cos2b)

        rel1 th3 = mZ2
                   + coeff
                   * (2 * sinth2 * sin th3 * cos th3 * (mH2 - mS2)
                      + 2 * sinth1 * costh2
                         * (mHSM2
                            - cos th3 ** 2 * mH2 - sin th3 ** 2 * mS2))
                   - lam2 * v2
        rel1' th3 = 4 * coeff * (mH2 - mS2)
                    * (sinth2 * (cos th3 ** 2 - sin th3 ** 2)
                      + sinth1 * costh2 * sin th3 * cos th3)
    in mpiHalf2piHalf <$> newton rel1 rel1' 0 1.0e-6

newton :: (Double -> Double)
       -> (Double -> Double)
       -> Double  -- ^ initial guess
       -> Double  -- ^ tolerance
       -> Maybe Double
newton f f' guess epsilon = newton' 100 guess
  where
    newton' :: Int -> Double -> Maybe Double
    newton' i guess0
        | i == 0    = Nothing
        | otherwise = let guess1 = guess0 - (f guess0 / f' guess0)
                          err =  abs (guess1 - guess0)
                      in if err < epsilon
                         then Just guess1
                         else newton' (i - 1) guess1

sincos :: Double -> (Double, Double)
sincos th = (sin th, cos th)

-- |
-- The second equation in (24) of
-- [arXiv:1211.0875](https://arxiv.org/abs/1211.0875).
getMu :: (Angle, Angle, Angle)  -- ^ (theta1, theta2, theta3)
      -> Double                 -- ^ lambda
      -> Double                 -- ^ tan(beta)
      -> Double
getMu (th1, th2, th3) lam tanb =
    let (mS2, mHSM2, mH2) = (mS * mS, mHSM * mHSM, mH * mH)
        (sinth1, costh1) = sincos th1
        (sinth2, costh2) = sincos th2
        (sinth3, costh3) = sincos th3
        !tan2b = 2 * tanb / (1 - tanb * tanb)

        term1 = - 0.5 * mHSM2 * costh1 ** 2 * sinth2 * costh2
        term2 = - 0.5 * (mH2 - mS2)
                * sinth1 * (costh2 ** 2 - sinth2 ** 2) * sinth3 * costh3
        term3 = 0.5 * ((mH2 - mS2 * sinth1 ** 2) * sinth3 ** 2
                       - (mH2 * sinth1 ** 2 - mS2) * costh3 ** 2)
                * sinth2 * costh2
        term4 = - 0.25 * tan2b
                * ((mH2 - mS2) * costh2 * 2 * sinth3 * costh3
                   - 2 * (mHSM2 - mH2 * costh3 ** 2
                          - mS2 * sinth3 ** 2) * sinth1 * sinth2)
                * costh1
    in (term1 + term2 + term3 + term4) / (lam * vEW)

-- |
-- The third equation in (24) of
-- [arXiv:1211.0875](https://arxiv.org/abs/1211.0875).
getLambda :: (Angle, Angle, Angle)  -- ^ (theta1, theta2, theta3)
          -> Double                 -- ^ lambda
          -> Double                 -- ^ tan(beta)
          -> Double
getLambda (th1, th2, th3) lam tanb =
    let (mS2, mHSM2, mH2) = (mS * mS, mHSM * mHSM, mH * mH)
        !tanbSq = tanb * tanb
        !cos2b = (1 - tanbSq) / (1 + tanbSq)

        rhs = - 0.5 / cos2b
              * ((mH2 - mS2) * cos th2 * 2 * sin th3 * cos th3
                 - 2 * (mHSM2 - mH2 * cos th3 ** 2 - mS2 * sin th3 ** 2)
                 * sin th1 * sin th2)
              * cos th1
    in rhs / (lam * vEW)
