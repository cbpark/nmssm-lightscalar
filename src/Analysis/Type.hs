{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

module Analysis.Type where

#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup                    ((<>))
#endif
import Data.ByteString.Builder
import Data.Double.Conversion.ByteString (toExponential, toFixed)

type Mass = Double
type Coupling = Double
type Angle = Double

data TreeLevelCouplings = TreeLevelCouplings { cTop    :: !Double
                                             , cBottom :: !Double
                                             , cVector :: !Double
                                             } deriving Show

renderTreeLevelCouplings :: TreeLevelCouplings -> Builder
renderTreeLevelCouplings TreeLevelCouplings {..} =
    convDbl cTop <> space <> convDbl cBottom <> space <> convDbl cVector

data LoopLevelCouplings = LoopLevelCouplings { cGluon :: !Double
                                             , cGamma :: !Double
                                             } deriving Show

renderLoopLevelCouplings :: LoopLevelCouplings -> Builder
renderLoopLevelCouplings LoopLevelCouplings {..} =
    convDbl cGluon <> space <> convDbl cGamma

data HiggsCoupling = HiggsCoupling { tree :: !TreeLevelCouplings
                                   , loop :: !LoopLevelCouplings
                                   } deriving Show

renderHiggsCoupling :: HiggsCoupling -> Builder
renderHiggsCoupling HiggsCoupling {..} =
    renderTreeLevelCouplings tree <> space <> renderLoopLevelCouplings loop

data MixingAngles = MixingAngles { theta1 :: !Double
                                 , theta2 :: !Double
                                 , theta3 :: !Double
                                 } deriving Show

renderMixingAngles :: MixingAngles -> Builder
renderMixingAngles MixingAngles {..} =
    convDbl theta1 <> space <> convDbl theta2 <> space <> convDbl theta3

data NMSSMParameters = NMSSMParameters { lambda    :: !Double
                                       , tanbeta   :: !Double
                                       , mu        :: !Double
                                       , bigLambda :: !Double
                                       } deriving Show

renderNMSSMParameters :: NMSSMParameters -> Builder
renderNMSSMParameters NMSSMParameters {..} =
    (byteString . toFixed 2) lambda <> space
    <> (byteString . toFixed 3) tanbeta <> space
    <> (byteString . toFixed 4) mu <> space
    <> (byteString . toFixed 4) bigLambda

data NMSSMSolution = NMSSMSolution { params     :: !NMSSMParameters
                                   , hCoupling  :: !HiggsCoupling
                                   , sCoupling  :: !HiggsCoupling
                                   , mixing     :: !MixingAngles
                                   , muCMSValue :: !Double
                                   , muLEPValue :: !Double
                                   } deriving Show

renderNMSSMSolution :: NMSSMSolution -> Builder
renderNMSSMSolution NMSSMSolution {..} =
    renderNMSSMParameters params <> space
    <> renderHiggsCoupling hCoupling <> space
    <> renderHiggsCoupling sCoupling <> space
    <> renderMixingAngles mixing <> space
    <> convDbl muCMSValue <> space <> convDbl muLEPValue <> charUtf8 '\n'

convDbl :: Double -> Builder
convDbl = byteString . toExponential 4

space :: Builder
space = stringUtf8 "  "
