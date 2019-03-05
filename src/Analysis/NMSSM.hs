module Analysis.NMSSM where

import Analysis.EFT.SignalStrength
import Analysis.NMSSM.Coupling     (couplingHSM, couplingHSM', couplingS)
import Analysis.NMSSM.Relations    (getLambda, getMu, getTheta3)
import Analysis.Type

import Control.Monad               (guard)

data NMSSMSolution = NMSSMSolution { params    :: NMSSMParameters
                                   , hCoupling :: HiggsCoupling
                                   , sCoupling :: HiggsCoupling
                                   , mixing    :: MixingAngles
                                   } deriving Show

searchNMSSM :: Double          -- ^ lambda
            -> Double          -- ^ tan(beta)
            -> (Angle, Angle)  -- ^ (theta1, theta2)
            -> Maybe NMSSMSolution
searchNMSSM lam tanb (th1, th2) = do
    let cH' = couplingHSM' (MixingAngles th1 th2 0) tanb

    -- check mu_{ZZ}(h) and mu_{bb}(h)
    guard $ (satisfyMuZZ13 . muVV) cH' && (satisfyMuBB13 . muBB) cH'

    th3 <- getTheta3 (th1, th2) lam tanb
    let muValue = getMu (th1, th2, th3) lam tanb
        bigLamValue = getLambda (th1, th2, th3) lam tanb
        mixingAngle = MixingAngles th1 th2 th3
        nmssmParams = NMSSMParameters { lambda    = lam
                                      , mu        = muValue
                                      , bigLambda = bigLamValue
                                      , tanbeta   = tanb
                                      }
        cH = couplingHSM mixingAngle (tree cH') nmssmParams

    -- check mu_{gamma gamma}(h)
    guard $ (satisfyMuGaGa13 . muGaGa) cH

    let cS = couplingS mixingAngle nmssmParams
    return $ NMSSMSolution { params    = nmssmParams
                           , hCoupling = cH
                           , sCoupling = cS
                           , mixing    = mixingAngle
                           }

calcMu :: (HiggsCoupling -> Double) -> NMSSMSolution -> Double
calcMu muF NMSSMSolution {sCoupling = cS} = muF cS

calcMuCMS, calcMuLEP :: NMSSMSolution -> Double
calcMuCMS = calcMu muCMS
calcMuLEP = calcMu muLEP
