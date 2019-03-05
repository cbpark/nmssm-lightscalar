module Main where

import Analysis.EFT.Coupling
import Analysis.SignalStrength

main :: IO ()
main = do
    print (couplingSM 125.0)
    print couplingHSM
    putStrLn $ "mu_VV(h) = "   ++ show (muVV couplingHSM)
    putStrLn $ "mu_bb(h) = "   ++ show (muBB couplingHSM)
    putStrLn $ "mu_gaga(h) = " ++ show (muGaGa couplingHSM)
