module Main (main) where

import           MultiScalarMultiplication (testMultiScalarMultiplication)
import           Prelude
import           ScalarSum                 (testScalarSum)

main :: IO ()
main = do
    testScalarSum
    testMultiScalarMultiplication
