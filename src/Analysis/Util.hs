module Analysis.Util (mkTheta12) where

import           Analysis.Type             (Angle)

import           Control.Monad             (replicateM)
import           Control.Monad.IO.Class    (MonadIO (..))
import           Control.Monad.ST          (runST)
import           Control.Monad.Trans.State (StateT, evalStateT, get, put)
import           Data.Vector.Unboxed       (Vector)
import qualified Data.Vector.Unboxed       as U
import           System.Random.MWC

mkTheta12 :: MonadIO m => Int -> m (Vector (Angle, Angle))
mkTheta12 n = do
    s <- liftIO (createSystemRandom >>= save)
    evalStateT (U.replicateM n thetaPair) s

thetaPair :: MonadIO m => StateT Seed m (Angle, Angle)
thetaPair = do
    s <- get
    let (th1, th2, s') = mkThetaPair s
    put s'
    return (th1, th2)
  where
    mkThetaPair s0 = runST $ do
        gen <- restore s0
        [t1, t2] <- replicateM 2 (uniformR (-pi/2, pi/2) gen)
        s1 <- save gen
        return (t1, t2, s1)
