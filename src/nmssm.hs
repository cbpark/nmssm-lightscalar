module Main where

import Analysis.EFT.Coupling
import Analysis.SignalStrength

main :: IO ()
main = do
    print (couplingSM 125.0)
    print couplingHSM
    putStrLn $ "mu_VV(h) = "   ++ show (muVVSM couplingHSM)
    putStrLn $ "mu_bb(h) = "   ++ show (mubbSM couplingHSM)
    putStrLn $ "mu_gaga(h) = " ++ show (muGaGaSM couplingHSM)
