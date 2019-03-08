{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module Main where

import           Analysis.NMSSM              (renderSolution, searchNMSSM)
import           Analysis.Util               (mkTheta12, parVectorChunk)

import           Control.Parallel.Strategies (using)
import           Data.ByteString.Builder
import           Data.ByteString.Char8       (ByteString)
import qualified Data.ByteString.Char8       as B
import           Data.Maybe                  (fromMaybe)
import qualified Data.Vector                 as V
import           Options.Generic
import           System.IO                   (IOMode (..), withBinaryFile)

main :: IO ()
main = do
    input <- unwrapRecord "Scan the parameter space of the NMSSM"
    let lam = lambda input
        tanb = tanbeta input
        n = fromMaybe 1000000 (np input)

    putStrLn $ "-- lambda = " ++ show lam ++ ", tan(beta) = " ++ show tanb ++ ":"

    theta12 <- mkTheta12 n
    let solutions = V.map (renderSolution . searchNMSSM lam tanb) theta12
                    `using` parVectorChunk 200

    let outfile = fromMaybe ("output_" ++ show lam  ++ "_" ++ show tanb ++ ".dat")
                  (output input)
    withBinaryFile outfile WriteMode $ \h -> do
        B.hPutStrLn h header
        V.mapM_ (hPutBuilder h) solutions

    putStrLn $ "-- " ++ outfile ++ " generated."

data InputArgs w = InputArgs
    { lambda  :: w ::: Double       <?> "lambda"
    , tanbeta :: w ::: Double       <?> "tan(beta)"
    , np      :: w ::: Maybe Int    <?> "number of parameter points"
    , output  :: w ::: Maybe String <?> "name of the output file"
    } deriving (Generic)

instance ParseRecord (InputArgs Wrapped)
deriving instance Show (InputArgs Unwrapped)

header :: ByteString
header = "#" <> " lambda" <> "   tanb" <> "        mu" <> "    Lambda"
         <> "      Ct(h)" <> "      Cb(h)" <> "      CV(h)"
         <> "      Cg(h)" <> "      Cga(h)"
         <> "      Ct(s)" <> "      Cb(s)" <> "      CV(s)"
         <> "      Cg(s)" <> "      Cga(s)"
         <> "       theta1" <> "      theta2" <> "     theta3"
         <> "     mu(CMS)" <> "     mu(LEP)"
