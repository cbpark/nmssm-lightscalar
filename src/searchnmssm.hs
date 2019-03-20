{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module Main where

import           Analysis.NMSSM            (searchNMSSM)
-- import           Analysis.Type               (renderNMSSMSolution)
import           Analysis.Util             (mkTheta12)

-- #if !(MIN_VERSION_base(4,11,0))
-- import           Data.Semigroup              ((<>))
-- #endif
-- import           Control.Parallel.Strategies (using)
-- import           Data.ByteString.Builder
-- import           Data.ByteString.Char8       (ByteString)
-- import qualified Data.ByteString.Char8       as B
import           Data.Maybe                (fromMaybe)
import qualified Data.Vector               as V
import           Options.Generic
-- import           System.IO                   (IOMode (..), withBinaryFile)
import           Control.Monad.Trans.State
import           System.Random.MWC

main :: IO ()
main = do
    input <- unwrapRecord "Scan the parameter space of the NMSSM"
    let r = rvalue input
        signMu = signum (musign input)
        n = fromMaybe 1000000 (np input)

    putStrLn $ "-- Set r = " ++ show r ++ ", sign(mu) = " ++ show signMu

    s0 <- createSystemRandom >>= save
    (theta12, s1) <- runStateT (mkTheta12 n) s0

    solutions <- evalStateT (V.mapM (searchNMSSM r signMu) theta12) s1
    V.mapM_ print solutions

    -- theta12 <- mkTheta12 n
    -- let solutons = V.map (renderNMSSMSolution . searchNMSSM r signMu) theta12
    --                `using` parVectorChunk 200

    -- let outfile = fromMaybe ("output_" ++ show r ++ ".dat") (output input)
    -- withBinaryFile outfile WriteMode $ \h -> do
    --     -- B.hPutStrLn h header
    --     V.mapM_ (hPutBuilder h) solutions

    -- putStrLn $ "-- " ++ outfile ++ " generated."
    -- return undefined

data InputArgs w = InputArgs
    { rvalue :: w ::: Double       <?> "r = lambda v / |mu|"
    , musign :: w ::: Double       <?> "the sign of mu"
    , np     :: w ::: Maybe Int    <?> "number of parameter points to try"
    , output :: w ::: Maybe String <?> "name of the output file"
    } deriving Generic

instance ParseRecord (InputArgs Wrapped)
deriving instance Show (InputArgs Unwrapped)

-- header :: ByteString
-- header = "#" <> " lambda" <> "   tanb" <> "       mh3"
--          <> "        mu" <> "    Lambda"
--          <> "      Ct(h)" <> "      Cb(h)" <> "      CV(h)"
--          <> "      Cg(h)" <> "      Cga(h)"
--          <> "      Ct(s)" <> "      Cb(s)" <> "      CV(s)"
--          <> "      Cg(s)" <> "      Cga(s)"
--          <> "       theta1" <> "      theta2" <> "     theta3"
--          <> "     mu(CMS)" <> "     mu(LEP)"
