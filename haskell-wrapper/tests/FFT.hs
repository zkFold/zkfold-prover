module FFT (testFFT) where

import qualified Data.Vector                                 as V
import           Prelude                                     hiding (Num (..), sum, take)
import           RustFunctions                               (rustMulFft)
import           Test.Hspec                                  (describe, hspec, it, shouldBe)
import           Test.QuickCheck                             (Testable (property))

import           ZkFold.Base.Algebra.Basic.Class             (MultiplicativeSemigroup ((*)))
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1)
import           ZkFold.Base.Algebra.EllipticCurve.Class     (EllipticCurve (ScalarField))
import           ZkFold.Base.Algebra.Polynomials.Univariate  (toPoly)

specFFT :: IO ()
specFFT = hspec $ do
    describe "Rust FFT multiplication specification" $ do
        it "should be equal to haskell" $ do
            property $
              \
               (l :: [ScalarField BLS12_381_G1])
               (r :: [ScalarField BLS12_381_G1])
                ->  let
                        left = toPoly $ V.fromList l
                        right = toPoly $ V.fromList r
                    in
                    toPoly (rustMulFft @(ScalarField BLS12_381_G1) (V.fromList l) (V.fromList r))
                    `shouldBe`
                    left * right


testFFT :: IO ()
testFFT = do
    specFFT
