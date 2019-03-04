module Analysis.NMSSM.Relations where

import Analysis.Data (mHH, mHSM, mS, mZ, vEW)
import Analysis.Type

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

getTheta3 :: (Angle, Angle)  -- ^ (theta1, theta2)
          -> Double          -- ^ lambda
          -> Double          -- ^ tan(beta)
          -> Maybe Double
getTheta3 (th1, th2) lam tanb =
    let (mZ2, mS2, mHSM2, mHH2) = (mZ * mZ, mS * mS, mHSM * mHSM, mHH * mHH)
        lam2 = lam * lam
        v2 = vEW * vEW

        (sinth1, costh1) = (sin th1, cos th1)
        (sinth2, costh2) = (sin th2, cos th2)
        coeff =  costh1 / sin (4 * atan tanb)

        rel1 th3 = mZ2
                   + coeff
                   * (2 * sinth2 * sin th3 * cos th3 * (mHH2 - mS2)
                      + 2 * sinth1 * costh2
                         * (mHSM2
                            - cos th3 ** 2 * mHH2 - sin th3 ** 2 * mS2))
                   - lam2 * v2
        rel1' th3 = 4 * coeff * (mHH2 - mS2)
                    * (sinth2 * (cos th3 ** 2 - sin th3 ** 2)
                      + sinth1 * costh2 * sin th3 * cos th3)
    in newton rel1 rel1' 0 1.0e-6
