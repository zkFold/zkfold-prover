module Main (main) where

import           FFT                       (testFFT)
import           MultiScalarMultiplication (testMultiScalarMultiplication)
import           Prelude
import           ScalarSum                 (testScalarSum)

main :: IO ()
main = do
    testScalarSum
    testFFT
    testMultiScalarMultiplication
