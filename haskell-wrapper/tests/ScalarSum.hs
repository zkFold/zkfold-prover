{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators       #-}
module ScalarSum (testScalarSum) where

import           BN254.BN254                                 (BN254_G1)
import           Data.Kind                                   (Type)
import           GHC.TypeNats                                (KnownNat)
import           Numeric.Natural                             (Natural)
import           Prelude                                     hiding (Num (..))
import           RustFunctions                               (rustScalarSum)
import           Test.Hspec                                  (describe, hspec, it, shouldBe)
import           Test.QuickCheck                             (Testable (property))

import           ZkFold.Base.Algebra.Basic.Class             (AdditiveSemigroup ((+)))
import           ZkFold.Base.Algebra.Basic.Field             (Zp)
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1)
import           ZkFold.Base.Algebra.EllipticCurve.Class     (EllipticCurve (ScalarField))

specScalarSum
    :: forall
        (a :: Type)
        (b :: Natural)
    .   ( ScalarField a ~ Zp b
        , KnownNat b
        )
    => String
    -> IO ()
specScalarSum name = hspec $ do
    describe ("Rust add specification for " <> name) $ do
        it "should be equal to haskell" $ do
            property $ \(x :: ScalarField a) (y :: ScalarField a) -> rustScalarSum @a @b x y `shouldBe` (x + y)

testScalarSum :: IO ()
testScalarSum = do
    specScalarSum @BLS12_381_G1 "BLS12_381_G1"
    specScalarSum @BN254_G1 "BN254"
