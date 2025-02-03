{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module RustBLS where

import           Control.DeepSeq                                   (NFData)
import           Control.Monad
import           Data.Bits
import           Data.Foldable                                     hiding (sum)
import           Data.Kind                                         (Type)
import qualified Data.Vector                                       as V
import           Data.Word
import           GHC.Generics                                      (Generic)
import           Prelude                                           hiding (Eq, Num (..), sum, (/), (^))
import qualified Prelude                                           as P
import           RustFunctions                                     (RustCore, both, rustDivFft, rustMulFft,
                                                                    rustMulPoint,
                                                                    rustMultiScalarMultiplicationWithoutSerialization)
import           Test.QuickCheck                                   (Arbitrary)

import           ZkFold.Base.Algebra.Basic.Class                   (sum)
import           ZkFold.Base.Algebra.Basic.Class                   hiding (sum)
import           ZkFold.Base.Algebra.Basic.Field
import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381       hiding (Fq, Fr)
import           ZkFold.Base.Algebra.EllipticCurve.Class
import           ZkFold.Base.Algebra.EllipticCurve.Pairing
import           ZkFold.Base.Algebra.Polynomials.Univariate
import           ZkFold.Base.Data.ByteString
import           ZkFold.Base.Protocol.NonInteractiveProof.Internal (CoreFunction (..), HaskellCore)
import           ZkFold.Symbolic.Data.Bool                         (BoolType)
import           ZkFold.Symbolic.Data.Conditional                  (Conditional)
import           ZkFold.Symbolic.Data.Eq                           (Eq)

type Fr = Zp BLS12_381_Scalar
type Fq = Zp BLS12_381_Base

data RustBLS12_381_G1
    deriving (Generic, NFData)

instance {-# OVERLAPPING #-} EllipticCurve RustBLS12_381_G1 where
    type ScalarField RustBLS12_381_G1 = Fr

    type BaseField RustBLS12_381_G1 = Fq

    pointGen = pointXY
        0x17f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb
        0x8b3f481e3aaa0f1a09e30ed741d8ae4fcf5e095d5d00af600db18cb2c04b3edd03cc744a2888ae40caa232946c5e7e1

    add = addPoints

    mul scalar point = rustMulPoint point scalar

instance WeierstrassCurve RustBLS12_381_G1 where
    weierstrassA = zero

    weierstrassB = fromConstant (4 :: Natural)

instance {-# OVERLAPPING #-} MultiplicativeSemigroup (Poly ((Zp BLS12_381_Scalar))) where
    -- | If it is possible to calculate a primitive root of unity in the field, proceed with FFT multiplication.
    -- Otherwise default to Karatsuba multiplication for polynomials of degree higher than 64 or use naive multiplication otherwise.
    -- 64 is a threshold determined by benchmarking.
    l * r = toPoly (rustMulFft (fromPoly l) (fromPoly r))

instance {-# OVERLAPPING #-} MultiplicativeMonoid (Poly ((Zp BLS12_381_Scalar))) where
    one = toPoly $ V.singleton one

instance {-# OVERLAPPING #-} (KnownNat size) => MultiplicativeSemigroup (PolyVec ( (Zp BLS12_381_Scalar)) size) where
    l * r = poly2vec $ toPoly $ rustMulFft (fromPoly $ vec2poly l) (fromPoly $ vec2poly r)

instance {-# OVERLAPPING #-} (KnownNat size) => MultiplicativeMonoid (PolyVec ( (Zp BLS12_381_Scalar)) size) where
    one = toPolyVec $ V.singleton one V.++ V.replicate (fromIntegral (value @size -! 1)) zero

instance {-# OVERLAPPING #-} (KnownNat size) => Semiring (PolyVec ( (Zp BLS12_381_Scalar)) size)

instance {-# OVERLAPPING #-} (KnownNat size) => Ring (PolyVec ( (Zp BLS12_381_Scalar)) size)

------------------------------------ BLS12-381 G2 ------------------------------------

data RustBLS12_381_G2
    deriving (Generic, NFData)

instance EllipticCurve RustBLS12_381_G2 where

    type ScalarField RustBLS12_381_G2 = Fr

    type BaseField RustBLS12_381_G2 = Fq2

    pointGen = pointXY
        (Ext2
            0x24aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8
            0x13e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e)
        (Ext2
            0xce5d527727d6e118cc9cdc6da2e351aadfd9baa8cbdd3a76d429a695160d12c923ac9cc3baca289e193548608b82801
            0x606c4a02ea734cc32acd2b02bc28b99cb3e287e85a763af267492ab572e99ab3f370d275cec1da1aaa9075ff05f79be)

    add = addPoints

    mul = pointMul

instance WeierstrassCurve RustBLS12_381_G2 where
    weierstrassA = zero

    weierstrassB = fromConstant (4 :: Natural)


instance Binary (Point RustBLS12_381_G1) where
    put (Point x y isInf) =
        if isInf then foldMap putWord8 (bitReverse8 (bit 1) : replicate 95 0)
                 else foldMap putWord8 (bytesOf 48 x <> bytesOf 48 y)
    get = do
        byte <- bitReverse8 <$> getWord8
        let compressed = testBit byte 0
            infinite = testBit byte 1
        if infinite then do
            skip (if compressed then 47 else 95)
            return pointInf
        else do
            let byteXhead = bitReverse8 $ clearBit (clearBit (clearBit byte 0) 1) 2
            bytesXtail <- replicateM 47 getWord8
            let x = ofBytes (byteXhead:bytesXtail)
                bigY = testBit byte 2
            if compressed then return (decompress (pointCompressed x bigY))
            else do
                bytesY <- replicateM 48 getWord8
                let y = ofBytes bytesY
                return (pointXY x y)

instance Binary (CompressedPoint RustBLS12_381_G1) where
    put (CompressedPoint x bigY isInf) =
        if isInf then foldMap putWord8 (bitReverse8 (bit 0 .|. bit 1) : replicate 47 0) else
        let
            flags = bitReverse8 $ if bigY then bit 0 .|. bit 2 else bit 0
            bytes = bytesOf 48 x
        in foldMap putWord8 ((flags .|. head bytes) : tail bytes)
    get = do
        byte <- bitReverse8 <$> getWord8
        let compressed = testBit byte 0
            infinite = testBit byte 1
        if infinite then do
            skip (if compressed then 47 else 95)
            return pointInf
        else do
            let byteXhead = bitReverse8 $ clearBit (clearBit (clearBit byte 0) 1) 2
            bytesXtail <- replicateM 47 getWord8
            let x = ofBytes (byteXhead:bytesXtail)
                bigY = testBit byte 2
            if compressed then return (pointCompressed x bigY)
            else do
                bytesY <- replicateM 48 getWord8
                let y :: Fq = ofBytes bytesY
                    bigY' = y > negate y
                return (pointCompressed x bigY')

instance Binary (Point RustBLS12_381_G2) where
    put (Point (Ext2 x0 x1) (Ext2 y0 y1) isInf) =
        if isInf then foldMap putWord8 (bitReverse8 (bit 1) : replicate 191  0) else
        let
            bytes = bytesOf 48 x1
              <> bytesOf 48 x0
              <> bytesOf 48 y1
              <> bytesOf 48 y0
        in
            foldMap putWord8 bytes
    get = do
        byte <- bitReverse8 <$> getWord8
        let compressed = testBit byte 0
            infinite = testBit byte 1
        if infinite then do
            skip (if compressed then 95 else 191)
            return pointInf
        else do
            let byteX1head = bitReverse8 $ clearBit (clearBit (clearBit byte 0) 1) 2
            bytesX1tail <- replicateM 47 getWord8
            bytesX0 <- replicateM 48 getWord8
            let x1 = ofBytes (byteX1head:bytesX1tail)
                x0 = ofBytes bytesX0
                bigY = testBit byte 2
            if compressed then return (decompress (pointCompressed (Ext2 x0 x1) bigY))
            else do
                bytesY1 <- replicateM 48 getWord8
                bytesY0 <- replicateM 48 getWord8
                let y0 = ofBytes bytesY0
                    y1 = ofBytes bytesY1
                return (pointXY (Ext2 x0 x1) (Ext2 y0 y1))

instance Binary (CompressedPoint RustBLS12_381_G2) where
    put (CompressedPoint (Ext2 x0 x1) bigY isInf) =
        if isInf then foldMap putWord8 (bitReverse8 (bit 0 .|. bit 1) : replicate 95 0) else
        let
            flags = bitReverse8 $ if bigY then bit 0 .|. bit 2 else bit 0
            bytes = bytesOf 48 x1 <> bytesOf 48 x0
        in
            foldMap putWord8 ((flags .|. head bytes) : tail bytes)
    get = do
        byte <- bitReverse8 <$> getWord8
        let compressed = testBit byte 0
            infinite = testBit byte 1
        if infinite then do
            skip (if compressed then 95 else 191)
            return pointInf
        else do
            let byteX1head = bitReverse8 $ clearBit (clearBit (clearBit byte 0) 1) 2
            bytesX1tail <- replicateM 47 getWord8
            bytesX0 <- replicateM 48 getWord8
            let x1 = ofBytes (byteX1head:bytesX1tail)
                x0 = ofBytes bytesX0
                x = Ext2 x0 x1
                bigY = testBit byte 2
            if compressed then return (pointCompressed (Ext2 x0 x1) bigY)
            else do
                bytesY1 <- replicateM 48 getWord8
                bytesY0 <- replicateM 48 getWord8
                let y0 = ofBytes bytesY0
                    y1 = ofBytes bytesY1
                    y :: Fq2 = Ext2 y0 y1
                    bigY' = y > negate y
                return (pointCompressed x bigY')

-- --------------------------------------- Pairing ---------------------------------------

-- -- | An image of a pairing is a cyclic multiplicative subgroup of @'Fq12'@
-- -- of order @'BLS12_381_Scalar'@.
newtype RustBLS12_381_GT = RustBLS12_381_GT Fq12
    deriving newtype (P.Eq, Show, MultiplicativeSemigroup, MultiplicativeMonoid)

instance Exponent RustBLS12_381_GT Natural where
    RustBLS12_381_GT a ^ p = RustBLS12_381_GT (a ^ p)

instance Exponent RustBLS12_381_GT Integer where
    RustBLS12_381_GT a ^ p = RustBLS12_381_GT (a ^ p)

deriving via (NonZero Fq12) instance MultiplicativeGroup RustBLS12_381_GT

instance Finite RustBLS12_381_GT where
    type Order RustBLS12_381_GT = BLS12_381_Scalar

instance  Pairing RustBLS12_381_G1 RustBLS12_381_G2 where
    type TargetGroup RustBLS12_381_G1 RustBLS12_381_G2 = RustBLS12_381_GT
    pairing a b
      = RustBLS12_381_GT
      $ finalExponentiation @BLS12_381_G2
      $ millerAlgorithmBLS12 param a b
      where
        param = [-1
          ,-1, 0,-1, 0, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0
          , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
          , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0
          , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
          ]

instance {-# OVERLAPPING #-} CoreFunction RustBLS12_381_G1 HaskellCore   where
    msm gs f = sum $ V.zipWith scale (fromPolyVec f) gs
    polyMul = (*)
    polyQr = qr

instance {-# OVERLAPPING #-} CoreFunction RustBLS12_381_G1 RustCore where
    msm gs f = uncurry rustMultiScalarMultiplicationWithoutSerialization (zipAndUnzip points scalars)
        where
            points = V.toList gs

            scalars = V.toList $ fromPolyVec f

            zipAndUnzip :: [a] -> [b] -> ([a],[b])
            zipAndUnzip (a:as) (b:bs)
                = let (rs1, rs2) = zipAndUnzip as bs
                    in
                    (a:rs1, b:rs2)
            zipAndUnzip _ _ = ([],[])

    polyMul x y = toPoly (rustMulFft @(ScalarField BLS12_381_G1) (fromPoly x) (fromPoly y))
    polyQr x y = both toPoly $ rustDivFft @(ScalarField BLS12_381_G1) (fromPoly x) (fromPoly y)
