module Analysis.LoopFuncs
    (
      loopFuncFermion
    , loopFuncFermion'
    , loopFuncVector
    , loopFuncVector'
    ) where

import Analysis.Type

import Data.Complex  (Complex (..), realPart)

loopFuncFermion :: Mass  -- ^ mass of the Higgs boson
                -> Mass  -- ^ mass of the fermion in the loop
                -> Complex Double
loopFuncFermion mh mf =
    let t = tau mh mf
        ft = liftCm (t - 1) * ftau t + liftCm t
    in liftCm (3 / (2 * t * t)) * ft

loopFuncFermion' :: Mass -> Mass -> Double
loopFuncFermion' mh mf = realPart (loopFuncFermion mh mf)

loopFuncVector :: Mass  -- ^ mass of the Higgs boson
               -> Mass  -- ^ mass of the vector boson in the loop
               -> Complex Double
loopFuncVector mh mv =
    let t = tau mh mv
        ft = liftCm (3 * (2 * t - 1)) * ftau t + liftCm (3 * t + 2 * t * t)
    in liftCm (1 / (7 * t * t)) * ft

loopFuncVector' :: Mass -> Mass -> Double
loopFuncVector' mh mf = realPart (loopFuncVector mh mf)

tau :: Mass -> Mass -> Double
tau mh mi = mh * mh / (4 * mi * mi)

ftau :: Double -> Complex Double
ftau t | t > 1     = let t1 = 1 / t
                         a = log ((1 + sqrt (1 - t1)) / (1 - sqrt (1 - t1)))
                               :+ (-pi)
                     in liftCm (-0.25) * a * a
       | otherwise = let a = liftCm $ asin (sqrt t)
                     in a * a

liftCm :: Double -> Complex Double
liftCm = (:+0)
