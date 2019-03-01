module Analysis.Type where

type Mass = Double
type Coupling = Double
type Angle = Double

data HiggsMass = HiggsMass { h1 :: Mass
                           , h2 :: Mass
                           , h3 :: Mass
                           } deriving Show

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
