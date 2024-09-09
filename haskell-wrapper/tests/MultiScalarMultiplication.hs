module MultiScalarMultiplication (testMultiScalarMultiplication) where

import qualified Data.Vector                                       as V
import           Prelude                                           hiding (Num (..), sum, take)
import           RustFunctions                                     (RustCore)
import           Test.Hspec                                        (describe, hspec, it, shouldBe)
import           Test.QuickCheck                                   (Testable (property))
import           Test.QuickCheck.Property                          (withMaxSuccess)

import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381       (BLS12_381_G1)
import           ZkFold.Base.Algebra.EllipticCurve.Class           (EllipticCurve (ScalarField), Point)
import           ZkFold.Base.Algebra.Polynomials.Univariate        (toPolyVec)
import           ZkFold.Base.Protocol.NonInteractiveProof.Internal (CoreFunction (msm), HaskellCore)


specMultiScalarMultiplication :: IO ()
specMultiScalarMultiplication = hspec $ do
    describe "Rust binary scalar mulitply specification" $ do
        it "should be equal to haskell" $ do
            property $ withMaxSuccess 100 $
              \
               (p :: [Point BLS12_381_G1])
               (s :: [ScalarField BLS12_381_G1])
                ->  let
                        points = V.fromList p
                        scalars = toPolyVec @(ScalarField BLS12_381_G1) @100 $ V.fromList s
                    in
                    msm @BLS12_381_G1 @RustCore points scalars
                    `shouldBe`
                    msm @BLS12_381_G1 @HaskellCore points scalars


testMultiScalarMultiplication :: IO ()
testMultiScalarMultiplication = do
    specMultiScalarMultiplication
