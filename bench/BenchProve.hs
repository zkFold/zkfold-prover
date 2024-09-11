module Main where

import qualified Data.ByteString                             as BS
import           Prelude                                     hiding (Num (..), length, sum, take, (-))
import           RustFunctions                               (RustCore)
import           Test.QuickCheck                             (Arbitrary (arbitrary), generate)
import           Test.Tasty.Bench

import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1, BLS12_381_G2)
import           ZkFold.Base.Protocol.ARK.Plonk              (Plonk)
import           ZkFold.Base.Protocol.NonInteractiveProof

type PlonkSizeBS = 128
type PlonkBS n = Plonk PlonkSizeBS n BLS12_381_G1 BLS12_381_G2 BS.ByteString

main :: IO ()
main = do
    (TestData a w) <- generate arbitrary :: IO (NonInteractiveProofTestData (PlonkBS 2) HaskellCore)

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
