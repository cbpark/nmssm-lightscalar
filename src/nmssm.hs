module Main where

import Analysis.EFT.Coupling

main :: IO ()
main = print (couplingSM 125.0)
