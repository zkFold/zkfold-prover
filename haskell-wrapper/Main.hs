{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- import System.Mem

-- import Data.Annotated

-- import Foreign.Rust.Serialisation.Raw

import ZkFold.Base.Algebra.EllipticCurve.Class (Point (..))
import ZkFold.Base.Algebra.EllipticCurve.BLS12_381

import Functions

main :: IO ()
main = do
    putStrLn "\n# Getting started\n"

    print $ rustWrapperAdd 1 2
