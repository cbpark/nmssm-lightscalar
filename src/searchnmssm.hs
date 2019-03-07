module Main where

import           Analysis.EFT.Coupling
import           Analysis.EFT.SignalStrength
import           Analysis.NMSSM              (renderSolution, searchNMSSM)
import           Analysis.Util

import           Control.Parallel.Strategies (using)
import           Data.ByteString.Builder
import qualified Data.Vector                 as V
import           System.Environment          (getArgs)
import           System.IO                   (IOMode (..), withBinaryFile)

main :: IO ()
main = do
    print (couplingSM 125.0)
    print couplingHSM
    putStrLn $ "mu_VV(h) = "   ++ show (muVV couplingHSM)
    putStrLn $ "mu_bb(h) = "   ++ show (muBB couplingHSM)
    putStrLn $ "mu_gaga(h) = " ++ show (muGaGa couplingHSM)

    n <- head <$> getArgs
    ts <- mkTheta12 (read n)
    -- V.mapM_ print ts

    let lam = 0.65; tanb = 2
        solutions = V.map (renderSolution . searchNMSSM lam tanb) ts
                    `using` parVectorChunk 200

    withBinaryFile "output.dat" WriteMode $ \h ->
        V.mapM_ (hPutBuilder h) solutions

    -- V.mapM_ (B.putStrLn . toLazyByteString) cs
    -- print $ length cs
    -- let output = renderNMSSMSolutions cs

    -- B.putStrLn $ toLazyByteString $ renderMixingAngles (MixingAngles 0.1 0.1 (-0.1))
    -- let Just sol = searchNMSSM 0.65 1.5 (0.01, -0.0001)
    -- print sol
    -- B.putStrLn $ toLazyByteString $ renderNMSSMSolution sol

    -- withBinaryFile "output.dat" WriteMode $ \h -> hPutBuilder h output
    -- return ()
