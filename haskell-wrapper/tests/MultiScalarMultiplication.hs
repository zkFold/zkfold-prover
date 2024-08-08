module MultiScalarMultiplication (testMultiScalarMultiplication) where

import           Data.Kind                                   (Type)
import           GHC.TypeNats                                (KnownNat)
import           Numeric.Natural                             (Natural)
import           Prelude                                     hiding (Num (..), sum)
import           RustFunctions                               (rustMultiScalarMultiplication)
import           Test.Hspec                                  (describe, hspec, it, shouldReturn)
import           Test.QuickCheck                             (Testable (property))

import           ZkFold.Base.Algebra.Basic.Class             (AdditiveSemigroup ((+)), sum)
import           ZkFold.Base.Algebra.Basic.Field             (Zp)
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Base, BLS12_381_G1, BLS12_381_Scalar)
import           ZkFold.Base.Algebra.EllipticCurve.Class     (EllipticCurve (BaseField, ScalarField, mul), Point)

specMultiScalarMultiplication
    :: forall
        (a :: Type)
        (b :: Natural)
        (c :: Natural)
    .   ( EllipticCurve a
        , ScalarField a ~ Zp b
        , KnownNat b
        , BaseField a ~ Zp c
        , KnownNat c
        )
    => IO ()
specMultiScalarMultiplication = hspec $ do
    describe "Rust binary scalar mulitply specification" $ do
        it "should be equal to haskell" $ do
            property $
              \
               (p1 :: Point a)
               (p2 :: Point a)
               (p3 :: Point a)
               (p4 :: Point a)
               (p5 :: Point a)
               (s1 :: ScalarField a)
               (s2 :: ScalarField a)
               (s3 :: ScalarField a)
               (s4 :: ScalarField a)
               (s5 :: ScalarField a)
                -> let scalars = [s1, s2, s3, s4, s5]
                       points  = [p1, p2, p3, p4, p5]
                   in
                    rustMultiScalarMultiplication @a @b @c points scalars
                    `shouldReturn`
                    sum (uncurry mul <$> zip scalars points)

testMultiScalarMultiplication :: IO ()
testMultiScalarMultiplication = do
    specMultiScalarMultiplication @BLS12_381_G1 @BLS12_381_Scalar @BLS12_381_Base
