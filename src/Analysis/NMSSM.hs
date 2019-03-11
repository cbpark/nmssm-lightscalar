{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}

module Analysis.NMSSM (searchNMSSM, renderSolution) where

import Analysis.EFT.SignalStrength
import Analysis.NMSSM.Coupling     (couplingHSM, couplingHSM', couplingS)
import Analysis.NMSSM.Relations    (getLambda, getMu, getTheta3)
import Analysis.Type

#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup              ((<>))
#endif
import Control.Monad               (guard)
import Data.ByteString.Builder     (Builder, stringUtf8)

searchNMSSM :: Double          -- ^ lambda
            -> Double          -- ^ tan(beta)
            -> (Angle, Angle)  -- ^ (theta1, theta2)
            -> Maybe NMSSMSolution
searchNMSSM lam tanb (th1, th2) = do
    let !cH' = couplingHSM' (MixingAngles th1 th2 0) tanb

    -- check mu_{ZZ}(h) and mu_{bb}(h)
    guard $ (satisfyMuZZ13 2 . muVV) cH' && (satisfyMuBB13 2 . muBB) cH'

    th3 <- getTheta3 (th1, th2) lam tanb
    let !muValue = getMu (th1, th2, th3) lam tanb

    -- check the LEP limit on chargino
    -- guard $ abs muValue > 103.5

    let bigLamValue = getLambda (th1, th2, th3) lam tanb
        mixingAngle = MixingAngles th1 th2 th3
        nmssmParams = NMSSMParameters { lambda    = lam
                                      , tanbeta   = tanb
                                      , mu        = muValue
                                      , bigLambda = bigLamValue
                                      }
        cH = couplingHSM mixingAngle (tree cH') nmssmParams

    -- check mu_{gamma gamma}(h)
    guard $ (satisfyMuGaGa13 2 . muGaGa) cH

    let !cS = couplingS mixingAngle nmssmParams
        !muCMSVal = muCMS cS
        !muLEPVal = muLEP cS

    -- check mu_{gamma gamma}(s) and mu_{bb}(s)
    -- guard $ satisfyMuCMS 2 muCMSVal
    -- guard $ satisfyMuLEP 2 muLEPVal
    guard $ satisfyMuCMS 2 muCMSVal || satisfyMuLEP 2 muLEPVal
    return $ NMSSMSolution { params    = nmssmParams
                           , hCoupling = cH
                           , sCoupling = cS
                           , mixing    = mixingAngle
                           , muCMSValue = muCMSVal
                           , muLEPValue = muLEPVal
                           }

renderSolution :: Maybe NMSSMSolution -> Builder
renderSolution Nothing  = mempty
renderSolution (Just s) = stringUtf8 "    " <> renderNMSSMSolution s
