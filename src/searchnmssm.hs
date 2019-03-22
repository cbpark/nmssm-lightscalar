{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module Main where

import           Analysis.NMSSM                   (renderSolution, searchNMSSM)
import           Analysis.Util                    (mkTheta12)

import           Control.Monad                    (when)
import           Control.Monad.Trans.State.Strict (evalStateT, runStateT)
import           Data.ByteString.Builder          (hPutBuilder)
import           Data.ByteString.Char8            (ByteString)
import qualified Data.ByteString.Char8            as B
import           Data.Maybe                       (fromMaybe)
-- #if !(MIN_VERSION_base(4,11,0))
import           Data.Semigroup                   ((<>))
-- #endif
import qualified Data.Vector                      as V
import           Options.Generic
import           System.Exit                      (die)
import           System.IO                        (IOMode (..), withBinaryFile)
import           System.Random.MWC                (createSystemRandom, save)

main :: IO ()
main = do
    input <- unwrapRecord "Scan the parameter space of the NMSSM"
    let r = rvalue input
        signMu = signum (musign input)
        n = fromMaybe 1000000 (np input)
        tanb = fromMaybe (-1) (tanbeta input)

    when (r < 0) $ die "The r value must be positive."
    when (signMu == 0) $ die "The sign of mu must be nonzero."
    putStrLn $ "-- Set r = " ++ show r ++ ", sign(mu) = " ++ show signMu

    (theta12, s) <- createSystemRandom >>= save >>= runStateT (mkTheta12 n)
    points <- evalStateT (V.mapM (searchNMSSM r signMu tanb) theta12) s
    let solutions = V.map renderSolution points
    -- V.mapM_ print solutions

    let outfile = fromMaybe ("output_r_" ++ show r ++
                             (if tanb > 0 then "_tanb_" ++ show tanb else "")
                             ++ ".dat") (output input)
    withBinaryFile outfile WriteMode $ \h -> do
        B.hPutStrLn h header
        V.mapM_ (hPutBuilder h) solutions

    putStrLn $ "-- " ++ outfile ++ " generated."

data InputArgs w = InputArgs
     { rvalue  :: w ::: Double       <?> "r = lambda v / |mu|"
     , musign  :: w ::: Double       <?> "the sign of mu"
     , tanbeta :: w ::: Maybe Double <?> "tan(beta)"
     , np      :: w ::: Maybe Int    <?> "number of parameter points to try"
     , output  :: w ::: Maybe String <?> "name of the output file"
     } deriving Generic

instance ParseRecord (InputArgs Wrapped)
deriving instance Show (InputArgs Unwrapped)

header :: ByteString
header = B.pack $ "# " <>
         foldl1 (\v1 v2 -> v1 <> ", " <> v2)
         (zipWith (\n v -> "(" <> show n <> ") " <> v) ([1..] :: [Int])
         [ "r", "lambda", "tanb", "mu", "Lambda", "mh3", "m0"
         , "Ct(h)", "Cb(h)", "CV(h)", "Cg(h)", "Cga(h)"
         , "Ct(s)", "Cb(s)", "CV(s)", "Cg(s)", "Cga(s)"
         , "theta1", "theta2", "theta3"
         , "mu(CMS)", "mu(LEP)"
         ])
