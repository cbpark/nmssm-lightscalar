module Main where

import           Analysis.EFT.Coupling
import           Analysis.EFT.SignalStrength
import           Analysis.NMSSM.Coupling     (mkCH)
import           Analysis.Util               (mkTheta12)

import qualified Data.Vector.Unboxed         as U

main :: IO ()
main = do
    print (couplingSM 125.0)
    print couplingHSM
    putStrLn $ "mu_VV(h) = "   ++ show (muVV couplingHSM)
    putStrLn $ "mu_bb(h) = "   ++ show (muBB couplingHSM)
    putStrLn $ "mu_gaga(h) = " ++ show (muGaGa couplingHSM)
    print $ mkCH (0.01, -0.0001) 0.65 1.5

    ts <- mkTheta12 10
    U.mapM_ print ts
