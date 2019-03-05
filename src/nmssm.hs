module Main where

import           Analysis.EFT.Coupling
import           Analysis.EFT.SignalStrength
import           Analysis.NMSSM              (searchNMSSM)
import           Analysis.Util               (mkTheta12)

import qualified Data.Vector                 as V

main :: IO ()
main = do
    print (couplingSM 125.0)
    print couplingHSM
    putStrLn $ "mu_VV(h) = "   ++ show (muVV couplingHSM)
    putStrLn $ "mu_bb(h) = "   ++ show (muBB couplingHSM)
    putStrLn $ "mu_gaga(h) = " ++ show (muGaGa couplingHSM)
    print $ searchNMSSM 0.65 1.5 (0.01, -0.0001)

    ts <- mkTheta12 10
    V.mapM_ print ts

    let lam = 0.65; tanb = 2
        cs = V.mapMaybe (searchNMSSM lam tanb) ts
    V.mapM_ print cs
