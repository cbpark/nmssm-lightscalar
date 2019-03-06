module Main where

import           Analysis.EFT.Coupling
import           Analysis.EFT.SignalStrength
import           Analysis.NMSSM              (searchNMSSM)
import           Analysis.Util

import           Control.Parallel.Strategies (using)
import qualified Data.Vector                 as V
import           System.Environment          (getArgs)

main :: IO ()
main = do
    print (couplingSM 125.0)
    print couplingHSM
    putStrLn $ "mu_VV(h) = "   ++ show (muVV couplingHSM)
    putStrLn $ "mu_bb(h) = "   ++ show (muBB couplingHSM)
    putStrLn $ "mu_gaga(h) = " ++ show (muGaGa couplingHSM)
    print $ searchNMSSM 0.65 1.5 (0.01, -0.0001)

    n <- head <$> getArgs
    ts <- mkTheta12 (read n)
    -- V.mapM_ print ts

    let lam = 0.65; tanb = 2
        -- cs = mapMaybe (searchNMSSM lam tanb) ts
        cs = catMaybes (V.map (searchNMSSM lam tanb) ts `using` parVectorChunk 200)
    -- V.mapM_ print cs
    print $ length cs
