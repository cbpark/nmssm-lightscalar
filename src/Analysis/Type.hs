{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData      #-}

module Analysis.Type where

#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup                    ((<>))
#endif
import Data.ByteString.Builder
import Data.Double.Conversion.ByteString (toExponential, toFixed)

newtype Mass = Mass Double deriving (Eq, Ord, Show)

renderMass :: Mass -> Builder
renderMass (Mass m) = (byteString . toFixed 3) m

massSq :: Mass -> Double
massSq (Mass m) = m * m

newtype Angle = Angle Double deriving Show

renderAngle :: Angle -> Builder
renderAngle (Angle th) = convDbl th

newtype TanBeta = TanBeta Double deriving Show

renderTanBeta :: TanBeta -> Builder
renderTanBeta (TanBeta tanb) = (byteString . toFixed 2) tanb

data TreeLevelCouplings
    = TreeLevelCouplings { cTop    :: Double
                         , cBottom :: Double
                         , cVector :: Double }
    | NullTreeLevelCouplings
    deriving Show

renderTreeLevelCouplings :: TreeLevelCouplings -> Builder
renderTreeLevelCouplings TreeLevelCouplings {..} =
    convDbl cTop <> space <> convDbl cBottom <> space <> convDbl cVector
renderTreeLevelCouplings NullTreeLevelCouplings  =
    convDbl 0    <> space <> convDbl 0       <> space <> convDbl 0

data LoopLevelCouplings
    = LoopLevelCouplings { cGluon :: Double
                         , cGamma :: Double }
    | NullLoopLevelCouplings
    deriving Show

renderLoopLevelCouplings :: LoopLevelCouplings -> Builder
renderLoopLevelCouplings LoopLevelCouplings {..} =
    convDbl cGluon <> space <> convDbl cGamma
renderLoopLevelCouplings NullLoopLevelCouplings =
    convDbl 0      <> space <> convDbl 0

nullLoopLevelCouplings :: LoopLevelCouplings
nullLoopLevelCouplings = LoopLevelCouplings 0 0

data HiggsCoupling
    = HiggsCoupling { tree :: TreeLevelCouplings
                    , loop :: LoopLevelCouplings }
    | NullHiggsCoupling
    deriving Show

renderHiggsCoupling :: HiggsCoupling -> Builder
renderHiggsCoupling HiggsCoupling {..} =
    renderTreeLevelCouplings tree <> space <> renderLoopLevelCouplings loop
renderHiggsCoupling NullHiggsCoupling  =
    renderTreeLevelCouplings NullTreeLevelCouplings <> space
    <> renderLoopLevelCouplings NullLoopLevelCouplings

data MixingAngles = MixingAngles { theta1 :: Angle
                                 , theta2 :: Angle
                                 , theta3 :: Angle
                                 } deriving Show

renderMixingAngles :: MixingAngles -> Builder
renderMixingAngles MixingAngles {..} =
    renderAngle theta1 <> space
    <> renderAngle theta2 <> space
    <> renderAngle theta3

data NMSSMParameters
    = NMSSMParameters { lambda    :: Double
                      , tanbeta   :: TanBeta
                      , mh3       :: Mass
                      , mu        :: Mass
                      , bigLambda :: Mass }
    | NullNMSSMParameters
    deriving Show

renderNMSSMParameters :: NMSSMParameters -> Builder
renderNMSSMParameters NMSSMParameters {..} =
    (byteString . toFixed 2) lambda <> space
    <> renderTanBeta tanbeta <> space
    <> renderMass mh3 <> space
    <> renderMass mu <> space
    <> renderMass bigLambda
renderNMSSMParameters NullNMSSMParameters  =
    (byteString . toFixed 2) 0 <> space
    <> renderTanBeta (TanBeta 0) <> space
    <> renderMass (Mass 0) <> space
    <> renderMass (Mass 0) <> space
    <> renderMass (Mass 0)

data NMSSMSolution = NMSSMSolution { rValue     :: Double
                                   , params     :: NMSSMParameters
                                   , hCoupling  :: HiggsCoupling
                                   , sCoupling  :: HiggsCoupling
                                   , mixing     :: MixingAngles
                                   , muCMSValue :: Double
                                   , muLEPValue :: Double
                                   } deriving Show

renderNMSSMSolution :: NMSSMSolution -> Builder
renderNMSSMSolution NMSSMSolution {..} =
    (byteString . toFixed 1) rValue <> space
    <> renderNMSSMParameters params <> space
    <> renderHiggsCoupling hCoupling <> space
    <> renderHiggsCoupling sCoupling <> space
    <> renderMixingAngles mixing <> space
    <> convDbl muCMSValue <> space <> convDbl muLEPValue
    <> charUtf8 '\n'

convDbl :: Double -> Builder
convDbl = byteString . toExponential 3

space :: Builder
space = stringUtf8 "  "
