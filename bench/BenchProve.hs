{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import qualified Data.ByteString                             as BS
import           GHC.Generics                                (U1 (U1))
import           Prelude                                     hiding (Num (..), length, sum, take, (-))
import           RustFunctions                               (RustCore)
import           Test.QuickCheck                             (Arbitrary (arbitrary), generate)
import           Test.QuickCheck.Arbitrary                   (Arbitrary1 (liftArbitrary))
import           Test.Tasty.Bench

import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1, BLS12_381_G2)
import           ZkFold.Base.Data.Vector                     (Vector)
import           ZkFold.Base.Protocol.NonInteractiveProof
import           ZkFold.Base.Protocol.Plonk                  (Plonk)

type PlonkBS n = Plonk U1 (Vector 1) 32 (Vector n) BLS12_381_G1 BLS12_381_G2 BS.ByteString

instance Arbitrary (U1 a) where
  arbitrary = return U1

instance Arbitrary1 U1 where
  liftArbitrary _ = return U1

main :: IO ()
main = do
    a <- generate arbitrary :: IO (PlonkBS 2)
    w <- generate arbitrary :: IO (Witness (PlonkBS 2))

    let spHaskell = setupProve @(PlonkBS 2) @HaskellCore a
        spRust    = setupProve @(PlonkBS 2) @RustCore    a

    defaultMain
        [
            bgroup "Prove group"

            [ bench "Haskell core" $ nf (show . uncurry (prove @(PlonkBS 2) @HaskellCore)) (spHaskell, w)
            , bcompare "Haskell core" $
                bench "Rust core" $ nf (show . uncurry (prove @(PlonkBS 2) @RustCore)) (spRust, w)
            ]
        ]
