module Main (main) where

import           MultiScalarMultiplication (testMultiScalarMultiplication)
import           ScalarSum                 (testScalarSum)

main :: IO ()
main = do
    testScalarSum
    testMultiScalarMultiplication
