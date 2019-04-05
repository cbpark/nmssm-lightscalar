{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf   #-}

module Analysis.NMSSM (searchNMSSM, renderSolution) where

import Analysis.EFT.Coupling            (correctSignYukawa)
import Analysis.EFT.SignalStrength
import Analysis.NMSSM.Coupling          (couplingH, couplingS)
import Analysis.NMSSM.Relations
import Analysis.Type
import Analysis.Util                    (genUniformValue, thetaPair)

import Control.Monad.IO.Class           (MonadIO (..))
import Control.Monad.Trans.State.Strict
import Data.ByteString.Builder          (Builder)
import Data.Maybe                       (fromMaybe)
import System.Random.MWC                (Seed)

searchNMSSM :: MonadIO m
            => Double  -- ^ r = \lambda v / |\mu|
            -> Double  -- ^ sign(\mu)
            -> Double  -- ^ tan(\beta)
            -> Int
            -> StateT Seed m (Maybe NMSSMSolution)
searchNMSSM r signMu tanbVal n
    | n == 0    = return Nothing
    | otherwise = do
          ((th1, th2), s0) <- get >>= runStateT thetaPair
          let (higgsResult, s1) = runState (searchHiggs r tanbVal (th1, th2)) s0

          case higgsResult of  -- satisfy the Higgs data?
              Nothing -> put s1 >> searchNMSSM r signMu tanbVal (n - 1)
              Just (tanb, cH) -> do
                  let (singletResult, s2) =
                          runState (searchSinglet r tanb (th1, th2)) s1
                  put s2

                  case singletResult of  -- satisfy the CMS and LEP excesses?
                      Nothing -> return $ nullResult (th1, th2) tanb cH
                      Just (th3, cS, muCMSVal, muLEPVal) -> do
                          let mixingAngles = MixingAngles th1 th2 th3

                          case getMH3 mixingAngles r signMu tanb of  -- mH3 is found?
                              Nothing -> return $ nullResult (th1, th2) tanb cH
                              Just mH3 -> do
                                  let Mass muVal =
                                          getMu mixingAngles r signMu tanb mH3
                                      nmssmParams =
                                          mkParams muVal tanb mH3 mixingAngles
                                  return . Just $ NMSSMSolution
                                                  { rValue     = r
                                                  , params     = nmssmParams
                                                  , hCoupling  = cH
                                                  , sCoupling  = cS
                                                  , mixing     = mixingAngles
                                                  , muCMSValue = muCMSVal
                                                  , muLEPValue = muLEPVal }
  where
    nullResult (th1', th2') tanb' cH' =
        let nullParam = NMSSMParameters { lambda    = Lambda 0
                                        , tanbeta   = tanb'
                                        , mu        = Mass 0
                                        , bigLambda = Mass 0
                                        , mh3       = Mass 0
                                        , m0        = Mass 0 }
        in Just $ NMSSMSolution { rValue     = r
                                , params     = nullParam
                                , hCoupling  = cH'
                                , sCoupling  = NullHiggsCoupling
                                , mixing     = MixingAngles th1' th2' (Angle 0)
                                , muCMSValue = 0
                                , muLEPValue = 0 }

    mkParams muVal' tanb' mH3' ang =
        let mu'        = Mass muVal'
            lambda'    = getLambda r mu'
            bigLambda' = getBigLambda ang lambda' tanb' mH3'
            m0'        = fromMaybe (Mass 0) (getM0 ang tanb' mH3')
        in NMSSMParameters { lambda    = lambda'
                           , tanbeta   = tanb'
                           , mu        = mu'
                           , bigLambda = bigLambda'
                           , mh3       = mH3'
                           , m0        = m0' }

searchHiggs :: Double -> Double -> (Angle, Angle)
            -> State Seed (Maybe (TanBeta, HiggsCoupling))
searchHiggs r tanb (th1, th2) = do
    s <- get
    let !mixingAngles = MixingAngles th1 th2 (Angle 0)
        (result, s')
            | tanb > 0 =
                  let tanb' = TanBeta tanb  -- from the user input
                      cH' = couplingH mixingAngles tanb' r
                     -- We will consider only correct-sign Yukawa coupling:
                     -- see eqs. (17) and (18) in arXiv:1211.0875
                  in if correctSignYukawa cH' && satisfyHiggsData cH'
                     then (Just (tanb', cH'), s)
                     else (Nothing,           s)  -- nothing!
            | otherwise = searchHiggs' 10000 s    -- better fix tanb ..

        searchHiggs' :: Int -> Seed -> (Maybe (TanBeta, HiggsCoupling), Seed)
        searchHiggs' n s0 =
            let (tanbVal, s1) = genUniformValue (1.5, 15) s0
                tanb' = TanBeta tanbVal
                !cH = couplingH mixingAngles tanb' r
            in if | satisfyHiggsData cH -> (Just (tanb', cH), s1)
                  | n == 0              -> (Nothing, s1)
                  | otherwise           -> searchHiggs' (n - 1) s1

    put s'
    return result
  where
    -- satisfyHiggsData c = (satisfyMuZZ13 2 . muVV) c
    --                       && (satisfyMuBB13 2 . muBB) c
    --                       && (satisfyMuGaGa13 2 . muGaGa) c
    satisfyHiggsData c = (   satisfyMuZZ          2 . muZZGGF      ) c
                         && (satisfyBrRatioGaGa   2 . brRatioGaGa  ) c
                         && (satisfyBrRatioTauTau 2 . brRatioTauTau) c
                         && (satisfyBrRatioBB     2 . brRatioBB    ) c

type SingletResult = Maybe (Angle, HiggsCoupling, Double, Double)

searchSinglet :: Double -> TanBeta -> (Angle, Angle) -> State Seed SingletResult
searchSinglet r tanb (th1, th2) = do
    let searchSinglet' ::
            Int -> Seed -> (SingletResult, Seed)
        searchSinglet' n s0 =
            let (th3Val, s1) = genUniformValue (-pi/2, pi/2) s0
                               -- genNormalValue (0.05, 0.1) s0
                th3' = Angle th3Val
                !cS = couplingS (MixingAngles th1 th2 th3') tanb r
                (muCMSVal', muLEPVal') = (,) <$> muCMS <*> muLEP $ cS
            in if | satisfyMuCMS 2 muCMSVal' && satisfyMuLEP 2 muLEPVal'
                              -> (Just (th3', cS, muCMSVal', muLEPVal'), s1)
                  | n == 0    -> (Nothing,                               s1)
                  | otherwise -> searchSinglet' (n - 1) s1

    (result, s) <- searchSinglet' 10000 <$> get
    put s
    return result

renderSolution :: Maybe NMSSMSolution -> Builder
renderSolution Nothing  = mempty
renderSolution (Just s) = renderNMSSMSolution s
