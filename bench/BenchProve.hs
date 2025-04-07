module Main where

import           Class
import qualified Data.ByteString                             as BS
import           Poly
import           Prelude
import           RustBLS                                     ()
import           Test.QuickCheck                             (Arbitrary (arbitrary), generate)
import           Test.Tasty.Bench

import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point, BLS12_381_G2_Point)
import           ZkFold.Base.Algebra.EllipticCurve.Class     (CyclicGroup (..))
import           ZkFold.Base.Algebra.Polynomials.Univariate  (PolyVec)
import           ZkFold.Base.Data.Vector                     (Vector (..))
import           ZkFold.Base.Protocol.NonInteractiveProof
import           ZkFold.Base.Protocol.Plonkup

type RustPlonkBS n = Plonkup (Vector 1) 32 (Vector n)
    Rust_BLS12_381_G1_Point
    Rust_BLS12_381_G2_Point
    BS.ByteString
    (RustPolyVec (ScalarFieldOf Rust_BLS12_381_G1_Point))

type HaskellPlonkBS n = Plonkup (Vector 1) 32 (Vector n)
    BLS12_381_G1_Point
    BLS12_381_G2_Point
    BS.ByteString
    (PolyVec (ScalarFieldOf BLS12_381_G1_Point))

main :: IO ()
main = do
  a  <- generate arbitrary :: IO (HaskellPlonkBS 2)
  a' <- generate arbitrary :: IO (RustPlonkBS 2)
  w  <- generate arbitrary :: IO (Witness (HaskellPlonkBS 2))
  w' <- generate arbitrary :: IO (Witness (RustPlonkBS 2))

  let spHaskell = setupProve @(HaskellPlonkBS 2) a
      spRust    = setupProve @(RustPlonkBS 2) a'

  defaultMain
    [
      bgroup "Prove group"

      [ bench "Haskell" $ nf (show . uncurry (prove @(HaskellPlonkBS 2))) (spHaskell, w)
      , bcompare "Haskell" $
          bench "Rust" $ nf (show . uncurry (prove @(RustPlonkBS 2))) (spRust, w')
      ]
    ]
