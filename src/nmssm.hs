module Main where

import Analysis.EFT.Coupling
import Analysis.NMSSM.Coupling (mkCH)
import Analysis.SignalStrength

main :: IO ()
main = do
    print (couplingSM 125.0)
    print couplingHSM
    putStrLn $ "mu_VV(h) = "   ++ show (muVV couplingHSM)
    putStrLn $ "mu_bb(h) = "   ++ show (muBB couplingHSM)
    putStrLn $ "mu_gaga(h) = " ++ show (muGaGa couplingHSM)
    print $ mkCH (0.01, -0.0001) 0.65 1.5
