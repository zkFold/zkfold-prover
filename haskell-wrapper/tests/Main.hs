module Main (main) where

import           FFT                       (testFFT)
import           MultiScalarMultiplication (testMultiScalarMultiplication)
import           Prelude

main :: IO ()
main = do
    testFFT
    testMultiScalarMultiplication
