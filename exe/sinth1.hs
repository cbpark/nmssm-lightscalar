{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module Main where

import           Analysis.EFT.SignalStrength (muZZGGF)
import           Analysis.NMSSM.Coupling     (couplingH)
import           Analysis.Type               hiding (tanbeta)

import           Control.Monad               (when)
import           Data.Maybe                  (fromMaybe)
import           Data.Vector                 (Vector)
import qualified Data.Vector                 as V
import           Options.Generic
import           System.Exit                 (die)
import           System.IO                   (IOMode (..), hPutStrLn, withFile)

main :: IO ()
main = do
    input <- unwrapRecord "sin(theta_1) and \\mu_{ZZ} in the NMSSM"
    let tanb = tanbeta input
        th2 = asin (sinth2 input)

    when (tanb <= 0) $ die "The tan(beta) value must be positive."

    let sinth2Val = sin th2
    putStrLn $ "# tan(beta) = " ++ show tanb
        ++ ", sin(theta_2) = " ++ show sinth2Val

    let points = V.map (getMuZZ (TanBeta tanb) th2) sinth1s

        outfile = fromMaybe
                  ("sinth1_muZZ_tanb_" ++ show tanb ++ "_sinth2_"
                   ++ show sinth2Val ++ ".dat") (output input)
    withFile outfile WriteMode $ \h ->
        V.mapM_ (\(sinth1, muZZ) -> hPutStrLn h $
                    show sinth1 ++ "  " ++ show muZZ) points

    putStrLn $ "-- " ++ outfile ++ " generated."
  where
    sinth1s :: Vector Double
    sinth1s = V.map (/ 100.0) $ V.enumFromN (-100) 201

data InputArgs w = InputArgs
     { tanbeta :: w ::: Double       <?> "tan(beta)"
     , sinth2  :: w ::: Double       <?> "sin(theta_2)"
     , output  :: w ::: Maybe String <?> "name of the output file"
     } deriving Generic

instance ParseRecord (InputArgs Wrapped)
deriving instance Show (InputArgs Unwrapped)

type SinTheta1 = Double
type BrRatio = Double

getMuZZ :: TanBeta -> Double -> Double -> (SinTheta1, BrRatio)
getMuZZ tanb th2 sinth1 =
    ( sinth1
    , muZZGGF $
      couplingH
      (MixingAngles (Angle (asin sinth1)) (Angle th2) (Angle 0)) tanb 0)
