module Analysis.Util
    (
      mkTheta12
    , parVectorChunk
    , catMaybes
    ) where

import           Analysis.Type                   (Angle)

-- import           Control.Monad                   (replicateM)
import           Control.Monad.IO.Class          (MonadIO (..))
import           Control.Monad.ST                (runST)
import           Control.Monad.Trans.State       (StateT, evalStateT, get, put)
import           Control.Parallel.Strategies
import           Data.Vector                     (Vector)
import qualified Data.Vector                     as V
import           System.Random.MWC
import           System.Random.MWC.Distributions (normal)

mkTheta12 :: MonadIO m => Int -> m (Vector (Angle, Angle))
mkTheta12 n = do
    s <- liftIO (createSystemRandom >>= save)
    evalStateT (V.replicateM n thetaPair) s

thetaPair :: MonadIO m => StateT Seed m (Angle, Angle)
thetaPair = do
    s <- get
    let (th1, th2, s') = mkThetaPair s
    put s'
    return (th1, th2)
  where
    mkThetaPair s0 = runST $ do
        gen <- restore s0
        -- [t1, t2] <- replicateM 2 (uniformR (-pi/2, pi/2) gen)
        -- [t1, t2] <- replicateM 2 (normal 0 0.01 gen)
        t1   <- normal 0   0.1 gen
        t2   <- normal 0.3 0.1 gen
        sign <- uniform gen
        s1 <- save gen
        return $ if (sign :: Bool) then (t1, t2, s1) else (t1, -t2, s1)

parVectorChunk :: Int -> Strategy (Vector a)
parVectorChunk n = fmap V.fromList . parListChunk n rseq . V.toList

catMaybes :: Vector (Maybe a) -> Vector a
catMaybes = V.concatMap maybeToVector
  where
    maybeToVector Nothing  = V.empty
    maybeToVector (Just x) = V.singleton x
