module Analysis.Type where

type Mass = Double
type Coupling = Double
type Angle = Double

data TreeLevelCouplings = TreeLevelCouplings { cTop    :: Double
                                             , cBottom :: Double
                                             , cVector :: Double
                                             } deriving Show

data LoopLevelCouplings = LoopLevelCouplings { cGluon :: Double
                                             , cGamma :: Double
                                             } deriving Show

data HiggsCoupling = HiggsCoupling { tree :: TreeLevelCouplings
                                   , loop :: LoopLevelCouplings
                                   } deriving Show

data NMSSMParameters = NMSSMParameters { lambda    :: Double
                                       , mu        :: Double
                                       , bigLambda :: Double
                                       , tanbeta   :: Double
                                       } deriving Show

data MixingAngles = MixingAngles { theta1 :: Double
                                 , theta2 :: Double
                                 , theta3 :: Double
                                 } deriving Show
