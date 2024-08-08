module RustFunctions
    ( rustScalarSum
    , rustMultiScalarMultiplication
    ) where

import qualified Data.ByteString                         as BS
import           Data.Kind                               (Type)
import           Foreign.Rust.Marshall.Variable          (withBorshVarBuffer)
import           Functions                               (rustWrapperMultiScalarMultiplication, rustWrapperScalarSum)
import           GHC.Natural                             (Natural)
import           GHC.TypeNats                            (KnownNat)
import           Numeric.Natural                         (Natural)
import           Pack                                    (packPoint, packScalar, unpackPoint, unpackScalar)

import           ZkFold.Base.Algebra.Basic.Field         (Zp, toZp)
import           ZkFold.Base.Algebra.EllipticCurve.Class (EllipticCurve (BaseField, ScalarField), Point)

rustScalarSum
    :: forall
        (a :: Type)
        (b :: Natural)
    .   ( EllipticCurve a
        , ScalarField a ~ Zp b
        , KnownNat b
        )
    => ScalarField a
    -> ScalarField a
    -> IO (ScalarField a)
rustScalarSum a b = do
    let a' = packScalar @a @b a
        b' = packScalar @a @b b
    sum' <- withBorshVarBuffer $ rustWrapperScalarSum a' b'
    pure $ unpackScalar @a @b sum'

rustMultiScalarMultiplication
    :: forall
        (a :: Type)
        (b :: Natural)
        (c :: Natural)
    .   ( EllipticCurve a
        , ScalarField a ~ Zp b
        , BaseField a ~ Zp c
        , KnownNat c
        )
    => [Point a]
    -> [ScalarField a]
    -> IO (Point a)
rustMultiScalarMultiplication points scalars = do
    let scalars' = mconcat $ packScalar @a @b <$> scalars
    let points'  = mconcat $ packPoint @a @c <$> points

    res' <- withBorshVarBuffer $ rustWrapperMultiScalarMultiplication points' scalars'
    pure $ unpackPoint res'
