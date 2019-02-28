module Analysis.Type where

type Mass = Double

data HiggsMass = HiggsMass
                 { h1 :: Mass
                 , h2 :: Mass
                 , h3 :: Mass
                 } deriving (Show)
