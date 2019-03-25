{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module Main where

import Control.Monad   (when)
import Options.Generic
import System.Exit     (die)

main :: IO ()
main = do
    input <- unwrapRecord "sin(theta_1) and \\mu_{\\gamma\\gamma} in the NMSSM"
    let r = rvalue input
        tanb = tanbeta input
        th2 = asin (sinth2 input)

    when (r < 0) $ die "The r value must be positive."
    putStrLn $ "-- Set r = " ++ show r ++ ", tan(beta) = " ++ show tanb
        ++ ", sin(theta_2) = " ++ show (sin th2)


data InputArgs w = InputArgs
     { rvalue  :: w ::: Double       <?> "r = lambda v / |mu|"
     , tanbeta :: w ::: Double       <?> "tan(beta)"
     , sinth2  :: w ::: Double       <?> "sin(theta_2)"
     , output  :: w ::: Maybe String <?> "name of the output file"
     } deriving Generic

instance ParseRecord (InputArgs Wrapped)
deriving instance Show (InputArgs Unwrapped)
