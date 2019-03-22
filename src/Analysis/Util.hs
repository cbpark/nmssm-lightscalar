{-# LANGUAGE BangPatterns #-}

module Analysis.Util
    (
      thetaPair
    , thetaPairs
    , genUniformValue
    , genNormalValue
    , mpiHalf2piHalf
    , sincos
    -- , parVectorChunk
    -- , catMaybes
    ) where

import           Analysis.Type                    (Angle (..))

-- import           Control.Monad                    (replicateM)
import           Control.Monad.IO.Class           (MonadIO)
import           Control.Monad.ST                 (runST)
import           Control.Monad.Trans.State.Strict
-- import           Control.Parallel.Strategies
import           Data.Vector                      (Vector)
import qualified Data.Vector                      as V
import           System.Random.MWC
import           System.Random.MWC.Distributions  (normal)

thetaPairs :: MonadIO m => Int -> StateT Seed m (Vector (Angle, Angle))
thetaPairs n = do
    (theta12s, s) <- get >>= runStateT (V.replicateM n thetaPair)
    put s
    return theta12s

thetaPair :: MonadIO m => StateT Seed m (Angle, Angle)
thetaPair = do
    (th1, th2, s) <- genThetaPair <$> get
    put s
    return (Angle th1, Angle th2)
  where
    genThetaPair s0 = runST $ do
        gen <- restore s0
        -- [t1, t2] <- replicateM 2 (uniformR (-pi/2, pi/2) gen)
        t1 <- mpiHalf2piHalf <$> normal 0 0.2 gen
        t2 <- uniformR (-pi/2, pi/2) gen
        -- t2 <- mpiHalf2piHalf <$> normal 0.15 0.2 gen
        -- t2 <- uniformR (-pi/2, pi/2) gen
        -- sign <- uniform gen
        s1 <- save gen
        return (t1, t2, s1)
        -- return $ if (sign :: Bool) then (t1, t2, s1) else (t1, -t2, s1)

genUniformValue :: (Double, Double) -> Seed -> (Double, Seed)
genUniformValue (limit0, limit1) s = runST $ do
    gen <- restore s
    x <- uniformR (limit0, limit1) gen
    s' <- save gen
    return (x, s')

genNormalValue :: (Double, Double) -> Seed -> (Double, Seed)
genNormalValue (mu, sigma) s = runST $ do
    gen <- restore s
    x <- normal mu sigma gen
    sign <- uniform gen
    s' <- save gen
    return $ if (sign :: Bool) then (x, s') else (-x, s')

-- | keep cos(th) to be positive.
mpiHalf2piHalf :: Double -> Double
mpiHalf2piHalf th | cos th < 0 = asin (- (sin th))
                  | otherwise  = asin (sin th)

sincos :: Angle -> (Double, Double)
sincos (Angle th) = let !sinth = sin th; !costh = cos th in (sinth, costh)

-- parVectorChunk :: Int -> Strategy (Vector a)
-- parVectorChunk n = fmap V.fromList . parListChunk n rseq . V.toList

-- catMaybes :: Vector (Maybe a) -> Vector a
-- catMaybes = V.concatMap maybeToVector
--   where
--     maybeToVector Nothing  = V.empty
--     maybeToVector (Just x) = V.singleton x
