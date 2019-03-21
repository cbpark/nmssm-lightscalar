{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf   #-}

module Analysis.NMSSM (searchNMSSM, renderSolution) where

import Analysis.Data               (mHSM)
import Analysis.EFT.SignalStrength
import Analysis.NMSSM.Coupling     (couplingH, couplingS)
import Analysis.NMSSM.Relations
import Analysis.Type
import Analysis.Util               (genUniformValue)

import Control.Monad.IO.Class      (MonadIO (..))
import Control.Monad.Trans.State
import Data.ByteString.Builder     (Builder)
import Data.Maybe                  (fromMaybe, isNothing)
import System.Random.MWC           (Seed)

searchNMSSM :: MonadIO m
            => Double          -- ^ r = \lambda v / |\mu|
            -> Double          -- ^ sign(\mu)
            -> Double          -- ^ tan(\beta)
            -> (Angle, Angle)  -- ^ (theta_1, theta_2)
            -> StateT Seed m (Maybe NMSSMSolution)
searchNMSSM r signMu tanbVal (th1, th2) = do
    (higgsResult, s) <- runState (searchHiggs r tanbVal (th1, th2)) <$> get

    if isNothing higgsResult
       then return Nothing
       else do
        -- liftIO $ print higgsResult
        let Just (tanb, cH) = higgsResult
            (singletResult, s') = runState (searchSinglet r tanb (th1, th2)) s

        put s'

        -- liftIO $ print singletResult
        let (th3, cS, muCMSVal, muLEPVal) =
                fromMaybe (Angle 0, NullHiggsCoupling, 0, 0) singletResult
            mixingAngles = MixingAngles th1 th2 th3
            mH3 = fromMaybe (Mass 0) (getMH3 mixingAngles r signMu tanb)

        if mH3 < mHSM
            then return Nothing  -- we consider only heavy Higgs
            else do
              let (mu', lambda', mH3', bigLambda') =
                      if isNothing singletResult
                      then (Mass 0, 0, Mass 0, Mass 0)
                      else ( getMu mixingAngles r signMu tanb mH3'
                           , getLambda r mu'
                           , mH3
                           , getBigLambda mixingAngles lambda' tanb mH3' )
                  nmssmParameters = NMSSMParameters { lambda  = lambda'
                                                    , tanbeta = tanb
                                                    , mh3     = mH3'
                                                    , mu      = mu'
                                                    , bigLambda = bigLambda'}
              -- liftIO $ print nmssmParameters
              return . Just $ NMSSMSolution { rValue     = r
                                            , params     = nmssmParameters
                                            , hCoupling  = cH
                                            , sCoupling  = cS
                                            , mixing     = mixingAngles
                                            , muCMSValue = muCMSVal
                                            , muLEPValue = muLEPVal }

searchHiggs :: Double -> Double -> (Angle, Angle)
            -> State Seed (Maybe (TanBeta, HiggsCoupling))
searchHiggs r tanb (th1, th2) = do
    s <- get
    let !mixingAngles = MixingAngles th1 th2 (Angle 0)
        (result, s') = if tanb > 0  -- from the input of user
                       then let tanb' = TanBeta tanb
                                cH' = couplingH mixingAngles tanb' r
                            in if satisfyHiggsData cH'
                               then (Just (tanb', cH'), s)
                               else (Nothing,           s)
                       else searchHiggs' 10000 s

        searchHiggs' :: Int -> Seed -> (Maybe (TanBeta, HiggsCoupling), Seed)
        searchHiggs' !n s0 =
            let (tanbVal, s1) = genUniformValue (1.5, 10) s0
                tanb' = TanBeta tanbVal
                !cH = couplingH mixingAngles tanb' r
            in if | satisfyHiggsData cH -> (Just (tanb', cH), s1)
                  | n == 0              -> (Nothing, s1)
                  | otherwise           -> searchHiggs' (n - 1) s1

    put s'
    return result
  where
    satisfyHiggsData c = (satisfyMuZZ13 2 . muVV) c
                          && (satisfyMuBB13 2 . muBB) c
                          && (satisfyMuGaGa13 2 . muGaGa) c

searchSinglet :: Double -> TanBeta -> (Angle, Angle)
              -> State Seed (Maybe (Angle, HiggsCoupling, Double, Double))
searchSinglet r tanb (th1, th2) = do
    let searchSinglet' ::
            Int -> Seed -> (Maybe (Angle, HiggsCoupling, Double, Double), Seed)
        searchSinglet' !n s0 =
            let (th3Val, s1) = genUniformValue (-pi/2, pi/2) s0
                th3' = Angle th3Val
                !cS = couplingS (MixingAngles th1 th2 th3') tanb r
                (muCMSVal', muLEPVal') = (,) <$> muCMS <*> muLEP $ cS
            in if | satisfyMuCMS 2 muCMSVal' && satisfyMuLEP 2 muLEPVal'
                              -> (Just (th3', cS, muCMSVal', muLEPVal'), s1)
                  | n == 0    -> (Nothing, s1)
                  | otherwise -> searchSinglet' (n - 1) s1

    (result, s) <- searchSinglet' 10000 <$> get
    put s
    return result

renderSolution :: Maybe NMSSMSolution -> Builder
renderSolution Nothing  = mempty
renderSolution (Just s) = renderNMSSMSolution s
