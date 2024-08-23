{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators       #-}
module RustFunctions
    ( rustScalarSum
    , rustMultiScalarMultiplication
    , RustCore
    ) where

import           Data.Kind                                   (Type)
import qualified Data.Vector                                 as V
import           Foreign.Rust.Marshall.Variable              (withPureBorshVarBuffer)
import           Functions                                   (rustWrapperMultiScalarMultiplication,
                                                              rustWrapperScalarSum)
import           GHC.Natural                                 (Natural)
import           GHC.TypeNats                                (KnownNat)
import           Pack                                        (packPoint, packScalar, unpackPoint, unpackScalar)
import           Prelude                                     hiding (sum)

import           ZkFold.Base.Algebra.Basic.Field             (Zp)
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Base, BLS12_381_G1, BLS12_381_Scalar)
import           ZkFold.Base.Algebra.EllipticCurve.Class     (EllipticCurve (BaseField, ScalarField), Point)
import           ZkFold.Base.Algebra.Polynomials.Univariate  (fromPolyVec)
import           ZkFold.Base.Protocol.NonInteractiveProof    (CoreFunction (..), msm)

rustScalarSum
    :: forall
        (a :: Type)
        (b :: Natural)
    .   ( ScalarField a ~ Zp b
        , KnownNat b
        )
    =>ScalarField a
    -> ScalarField a
    -> ScalarField a
rustScalarSum a b = unpackScalar @a @b sum'
    where
        a' = packScalar @a @b a
        b' = packScalar @a @b b
        sum' = withPureBorshVarBuffer $ rustWrapperScalarSum a' b'

rustMultiScalarMultiplication
    :: forall
        (a :: Type)
        (b :: Natural)
        (c :: Natural)
    .   ( ScalarField a ~ Zp b
        , BaseField a ~ Zp c
        , KnownNat c
        )
    =>[Point a]
    -> [ScalarField a]
    -> Point a
rustMultiScalarMultiplication points scalars = unpackPoint res'
    where
        scalars' = mconcat $ packScalar @a @b <$> scalars
        points'  = mconcat $ packPoint @a @c <$> points
        res' = withPureBorshVarBuffer $ rustWrapperMultiScalarMultiplication points' scalars'


data RustCore

instance CoreFunction BLS12_381_G1 RustCore where
    msm gs f = uncurry (rustMultiScalarMultiplication @BLS12_381_G1 @BLS12_381_Scalar @BLS12_381_Base) (zipAndUnzip points scalars)
        where
            points = V.toList gs

            scalars = V.toList $ fromPolyVec f

            zipAndUnzip :: [a] -> [b] -> ([a],[b])
            zipAndUnzip (a:as) (b:bs)
                = let (rs1, rs2) = zipAndUnzip as bs
                    in
                    (a:rs1, b:rs2)
            zipAndUnzip _ _ = ([],[])


