module FFT (testFFT) where

import           Control.Monad                               (unless)
import qualified Data.Vector                                 as V
import           Prelude                                     hiding (Num (..), sum, take)
import           RustFunctions                               (rustDivFft, rustMulFft)
import           Test.Hspec                                  (describe, hspec, it, shouldBe)
import           Test.QuickCheck                             (Testable (property))

import           ZkFold.Base.Algebra.Basic.Class             (AdditiveMonoid (zero), MultiplicativeMonoid (one),
                                                              MultiplicativeSemigroup ((*)))
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1, BLS12_381_GT (BLS12_381_GT))
import           ZkFold.Base.Algebra.EllipticCurve.Class     (EllipticCurve (ScalarField))
import           ZkFold.Base.Algebra.Polynomials.Univariate  (deg, qr, toPoly)

specMulFFT :: IO ()
specMulFFT = hspec $ do
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

specDivFFT :: IO ()
specDivFFT = hspec $ do
    describe "Rust FFT division specification" $ do
        it "should be equal to haskell" $ do
            property $
              \
               (l :: [ScalarField BLS12_381_G1])
               (r :: [ScalarField BLS12_381_G1])
                ->  let
                        left = toPoly $ V.fromList l
                        right = toPoly $ V.fromList r
                        (ll, rr) = (rustDivFft @(ScalarField BLS12_381_G1) (V.fromList l) (V.fromList r))
                    in (unless (deg right == - 1) $ (toPoly ll, toPoly rr) `shouldBe` left `qr` right)


testFFT :: IO ()
testFFT = do
    specDivFFT
    specMulFFT
