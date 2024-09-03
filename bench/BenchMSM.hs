{-# LANGUAGE AllowAmbiguousTypes #-}
module Main where

import qualified Data.Vector                                 as V
import           Prelude                                     hiding (Num (..), length, sum, take, (-))
import           RustFunctions                               (RustCore)
import           Test.QuickCheck                             (Arbitrary (arbitrary), generate)
import           Test.Tasty.Bench

import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1)
import           ZkFold.Base.Algebra.EllipticCurve.Class     (EllipticCurve (ScalarField), Point)
import           ZkFold.Base.Algebra.Polynomials.Univariate  (PolyVec, toPolyVec)
import           ZkFold.Base.Data.Vector                     (Vector (..))
import           ZkFold.Base.Protocol.NonInteractiveProof

testMSM :: forall core size . (CoreFunction BLS12_381_G1 core) => V.Vector (Point BLS12_381_G1) -> PolyVec (ScalarField BLS12_381_G1) size -> Bool
testMSM points scalars = let !_ = msm @BLS12_381_G1 @core points scalars in True

type Length = 1024

main :: IO ()
main = do
    (Vector p) <- generate arbitrary :: IO (Vector Length (Point BLS12_381_G1))
    (Vector s) <- generate arbitrary :: IO (Vector Length (ScalarField BLS12_381_G1))

    let
        points = V.fromList p
        scalars = toPolyVec @(ScalarField BLS12_381_G1) @Length $ V.fromList s

    defaultMain
        [
            bgroup "MSM group"
            [
                bench "Haskell msm" $ nf (uncurry (testMSM @HaskellCore)) (points, scalars),
                bcompare "Haskell msm" $
                    bench "Rust-arkmsm" $ nf (uncurry (testMSM @RustCore)) (points, scalars)
            ]
        ]
