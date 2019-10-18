{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module Main where

import Analysis.Data   (vEW)

import Data.Maybe      (fromMaybe)
import Options.Generic
import System.IO       (IOMode (..), hPutStrLn, withFile)

main :: IO ()
main = do
    input <- unwrapRecord "epsilon vs X_t / M_S"
    let yVal = yms input
        mstop = fromMaybe 1000 (ms input)

        npoints = 10000
        range = 3.0
        xVals = map (* (range / npoints)) [-npoints .. npoints]
        es = zip xVals (map (eps mstop yVal) xVals)

        outfile = "epsilon_t_" ++ show yVal ++ ".dat"

    withFile outfile WriteMode $ \h ->
        mapM_ (\(xv, e) -> hPutStrLn h $ show xv ++ "  " ++ show e) es
    putStrLn $ "-- " ++ outfile ++ " generated."

data InputArgs w =
    InputArgs { yms :: w ::: Double       <?> "Y_t / M_S"
              , ms  :: w ::: Maybe Double <?> "M_S"
              } deriving Generic

instance ParseRecord (InputArgs Wrapped)
deriving instance Show (InputArgs Unwrapped)

eps :: Double -> Double -> Double -> Double
eps m y x =
    let term1 = log (m*m / mt / mt)
        term2 = x * (x + y) / 2 - x ** 3 * y / 12
        coeff = 3 * yt ** 4 / (4 * pi ** 2)
    in coeff * (term1 + term2)

mt, yt :: Double
mt = 160
yt = mt / vEW
