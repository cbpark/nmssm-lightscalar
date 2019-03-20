{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

module Analysis.NMSSM.Relations
    (
      getMH3
    , getMu
    , getLambda
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

        f mHSq = (/ (-r)) . (* signMu) $
                 mZ2 + coeff * (2 * sinth2 * sinth3 * costh3 * (mHSq - mS2)
                                + 2 * sinth1 * costh2
                                   * (mHSM2
                                      - costh3 ** 2 * mHSq - sinth3 ** 2 * mS2))

        f' _ = (/ (-r)) . (* signMu) $
               coeff * (2 * sinth2 * sinth3 * costh3
                        - 2 * sinth1 * costh2 * costh3 ** 2)
    in Rel f f'

eqG :: MixingAngles -> TanBeta -> Rel
eqG (MixingAngles th1 th2 th3) (TanBeta tanb) =
    let (!mS2, !mHSM2) = (massSq mS, massSq mHSM)
        (sinth1, costh1) = sincos th1
        (sinth2, costh2) = sincos th2
        (sinth3, costh3) = sincos th3

        !tan2b = 2 * tanb / (1 - tanb * tanb)

        g1 _    = - 0.5 * mHSM2 * costh1 ** 2 * sinth2 * costh2
        g2 mHSq = - 0.5 * (mHSq - mS2)
                  * sinth1 * (costh2 ** 2 - sinth2 ** 2) * sinth3 * costh3
        g3 mHSq = 0.5 * ((mHSq - mS2 * sinth1 ** 2) * sinth3 ** 2
                         - (mHSq * sinth1 ** 2 - mS2) * costh3 ** 2)
                  * sinth2 * costh2
        g4 mHSq = - 0.25 * tan2b
                  * ((mHSq - mS2) * costh2 * 2 * sinth3 * costh3
                     - 2 * (mHSM2 - mHSq * costh3 ** 2
                            - mS2 * sinth3 ** 2) * sinth1 * sinth2)
                  * costh1
        g mHSq = sum $ map ($ mHSq) [g1, g2, g3, g4]

        g1' _ = 0
        g2' _ = - 0.5 * sinth1 * (costh2 ** 2 - sinth2 ** 2) * sinth3 * costh3
        g3' _ = 0.5 * (sinth3 ** 2 - sinth1 ** 2 * costh3 ** 2) * sinth2 * costh2
        g4' _ = - 0.25 * tan2b
                  * (costh2 * 2 * sinth3 * costh3
                     + 2 * costh3 ** 2 * sinth1 * sinth2) * costh1

        g' mHSq = sum $ map ($ mHSq) [g1', g2', g3', g4']
    in Rel g g'

getMH3 :: MixingAngles
       -> Double  -- ^ r = \lambda v / |\mu|
       -> Double  -- ^ sign(\mu)
       -> TanBeta
       -> Maybe Mass
getMH3 ang r signMu tanb =
    let eq = eqF ang r signMu tanb <> eqG ang tanb
    in case newton eq 1.0 1.0e-6 of
           Just mH3Sq -> if mH3Sq < 0  -- something went wrong!
                         then Nothing
                         else Just $ Mass (sqrt mH3Sq)
           _          -> Nothing

getMu :: MixingAngles -> Double -> Double -> TanBeta -> Mass -> Double
getMu ang r signMu tanb (Mass mH3) =
    let Rel g _ = eqG ang tanb
        !mH3Sq = mH3 * mH3
        !muSq =  g mH3Sq / r
    in signMu * if muSq >= 0 then sqrt muSq else sqrt (- muSq)

getLambda :: Double -> Double -> Double
getLambda r muVal = r * abs muVal / vEW

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
