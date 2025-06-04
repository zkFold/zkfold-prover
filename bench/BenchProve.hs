module Main where

import qualified Data.ByteString                        as BS
import           GHC.Generics                           (Par1 (..))
import           Poly
import           Prelude
import           RustBLS                                ()
import           Test.QuickCheck                        (Arbitrary (arbitrary), generate)
import           Test.Tasty.Bench
import           Types

import           ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point, BLS12_381_G2_Point)
import           ZkFold.Algebra.EllipticCurve.Class     (CyclicGroup (..))
import           ZkFold.Algebra.Polynomial.Univariate   (PolyVec)
import           ZkFold.Data.Vector                     (Vector (..))
import           ZkFold.Protocol.NonInteractiveProof
import           ZkFold.Protocol.Plonkup

type RustPlonkBS = Plonkup (Vector 2) Par1 32
    Rust_BLS12_381_G1_Point
    Rust_BLS12_381_G2_Point
    BS.ByteString
    (RustPolyVec (ScalarFieldOf Rust_BLS12_381_G1_Point))

type HaskellPlonkBS = Plonkup (Vector 2) Par1 32
    BLS12_381_G1_Point
    BLS12_381_G2_Point
    BS.ByteString
    (PolyVec (ScalarFieldOf BLS12_381_G1_Point))

main :: IO ()
main = do
  a  <- generate arbitrary :: IO HaskellPlonkBS
  a' <- generate arbitrary :: IO RustPlonkBS
  w  <- generate arbitrary :: IO (Witness HaskellPlonkBS)
  w' <- generate arbitrary :: IO (Witness RustPlonkBS)

  let spHaskell = setupProve @HaskellPlonkBS a
      spRust    = setupProve @RustPlonkBS a'

  defaultMain
    [
      bgroup "Prove group"

      [ bench "Haskell" $ nf (show . uncurry (prove @HaskellPlonkBS)) (spHaskell, w)
      , bcompare "Haskell" $
          bench "Rust" $ nf (show . uncurry (prove @RustPlonkBS)) (spRust, w')
      ]
    ]
