module Main where

import Analysis.EFT.Coupling
import Analysis.SignalStrength

main :: IO ()
main = do
    print (couplingSM 125.0)
    print couplingHSM
    putStrLn $ "mu_VV = " ++ show (muVVSM couplingHSM)
