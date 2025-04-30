{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module RustBLS where

import           Control.Monad
import           Conversion
import qualified Data.Vector                            as V
import           Foreign
import           Foreign.C.Types
import           GHC.Base
import           GHC.IO                                 (unsafePerformIO)
import           GHC.Natural                            (naturalToInteger)
import           Poly
import           Prelude                                hiding (fromIntegral, negate, (+), (-), (^))
import qualified Prelude                                as P
import           RustFunctions
import           System.Posix.DynamicLinker
import           Test.QuickCheck                        hiding (scale)
import           Types

import           ZkFold.Algebra.Class                   hiding (sum)
import qualified ZkFold.Algebra.EllipticCurve.BLS12_381 as EC
import           ZkFold.Algebra.EllipticCurve.BLS12_381 hiding (Fq, Fr)
import           ZkFold.Algebra.EllipticCurve.Class
import           ZkFold.Algebra.Number
import           ZkFold.Algebra.Polynomial.Univariate
import           ZkFold.Data.ByteString
import           ZkFold.Symbolic.MonadCircuit

pointSize :: Int
pointSize = sizeOf (undefined :: Rust_BLS12_381_G1_Point)

scalarSize :: Int
scalarSize = sizeOf (undefined :: ScalarFieldOf Rust_BLS12_381_G1_Point)

------------------------------------- Fr --------------------------------------

instance ToConstant Fr where
  type Const Fr = Natural
  toConstant = toConstant . r2h

instance ResidueField Fr where
  type IntegralOf Fr = Integer
  fromIntegral = h2r . fromIntegral
  toIntegral = toIntegral . r2h

instance Binary Fr where
  put = put . r2h
  get = h2r <$> get

instance Exponent BLS12_381_GT Fr where
  a ^ p = a ^ (r2h p)

instance Eq Fr where
  (==) a b = (r2h a) == (r2h b)

instance Ord Fr where
  a <= b = (<=) (r2h a) (r2h b)

instance Exponent Fr Natural where
  (^) a b = h2r $ (^) (r2h a) b

instance Exponent Fr Integer where
  (^) a b = h2r $ (^) (r2h a) b

instance AdditiveSemigroup Fr where
  (+) a b = h2r $ (+) (r2h a) (r2h b)

instance AdditiveMonoid Fr where
  zero = h2r zero

instance AdditiveGroup Fr where
  (-) a b = h2r $ (-) (r2h a) (r2h b)

  negate a = h2r $ negate (r2h a)

instance MultiplicativeSemigroup Fr where
  (*) a b = unsafePerformIO runMul
    where
      runMul :: IO Fr
      runMul = do
        dl <- dlopen libPath [RTLD_NOW]
        mulPtr <- dlsym dl "rust_wrapper_mul"
        let !mulf = mkFunMul $ castFunPtr mulPtr

        out <- callocForeignPtrBytes @CChar scalarSize
        withForeignPtr (rawData $ rawScalar a) $ \ptr1 -> do
          withForeignPtr (rawData $ rawScalar b) $ \ptr2 -> do
            withForeignPtr out $ \outPtr -> do
              mulf
                (castPtr ptr1) scalarSize
                (castPtr ptr2) scalarSize
                scalarSize (castPtr outPtr)

        dlclose dl
        return $ RScalar $ RData $ out

instance MultiplicativeMonoid Fr where
  one = h2r one

instance FromConstant Natural Fr where
  fromConstant = h2r . fromConstant

instance FromConstant Integer Fr where
  fromConstant = h2r . fromConstant

instance Scale Natural Fr where
  scale a b = h2r $ scale a (r2h b)

instance Scale Integer Fr where
  scale a b = h2r $ scale a (r2h b)

instance Semiring Fr where

instance Ring Fr where

instance Field Fr where
  (//) a b = h2r $ (//) (r2h a) (r2h b)

  finv x = h2r $ finv (r2h x)

  rootOfUnity n = do
    !a <- rootOfUnity n :: Maybe EC.Fr
    return $ h2r a

instance Arbitrary Fr where
  arbitrary = h2r <$> arbitrary

instance Show Fr where
  show = show . r2h

------------------------------------ BLS12-381-G1 --------------------------------------

instance EllipticCurve Rust_BLS12_381_G1_Point where
  type CurveOf Rust_BLS12_381_G1_Point = "Rust BLS12-381-G1"
  type BaseFieldOf Rust_BLS12_381_G1_Point = EC.Fq

  isOnCurve w = isOnCurve $ r2h w

instance Planar EC.Fq Rust_BLS12_381_G1_Point where
  pointXY a b = h2r $ pointXY a b

instance Binary Rust_BLS12_381_G1_CompressedPoint where
  put = put . r2h
  get = h2r <$> get

instance Compressible Rust_BLS12_381_G1_Point where
    type Compressed Rust_BLS12_381_G1_Point = Rust_BLS12_381_G1_CompressedPoint
    pointCompressed = error "Not implemented: pointCompressed"
    compress (RPoint w) = RPoint w
    decompress (RPoint w) = RPoint w

instance RustHaskell Rust_BLS12_381_G1_CompressedPoint BLS12_381_G1_CompressedPoint where
  r2h = compress @BLS12_381_G1_Point . r2h . decompress @Rust_BLS12_381_G1_Point

  h2r = compress @Rust_BLS12_381_G1_Point . h2r . decompress @BLS12_381_G1_Point

instance Pairing Rust_BLS12_381_G1_Point Rust_BLS12_381_G2_Point BLS12_381_GT where
  pairing a b = pairing (r2h a) (r2h b)


instance Scale Natural Rust_BLS12_381_G1_Point where
  scale a b = h2r $ scale a (r2h b)

instance Scale Integer Rust_BLS12_381_G1_Point where
  scale a b = h2r $ scale a (r2h b)

instance AdditiveSemigroup Rust_BLS12_381_G1_Point where
  a + b = unsafePerformIO runSum
    where
      runSum :: IO Rust_BLS12_381_G1_Point
      runSum = do
        dl <- dlopen libPath [RTLD_NOW]
        sumPtr <- dlsym dl "rust_wrapper_sum"
        let !sumf = mkFunSum $ castFunPtr sumPtr

        out <- callocForeignPtrBytes @CChar pointSize
        withForeignPtr (rawData $ rawPoint a) $ \ptr1 -> do
          withForeignPtr (rawData $ rawPoint b) $ \ptr2 -> do
            withForeignPtr out $ \outPtr -> do
              sumf
                (castPtr ptr1) pointSize
                (castPtr ptr2) pointSize
                pointSize (castPtr outPtr)

        dlclose dl
        return $ RPoint $ RData $ out

instance AdditiveMonoid Rust_BLS12_381_G1_Point where
  zero = h2r zero

instance AdditiveGroup Rust_BLS12_381_G1_Point where
  (-) a b = h2r $ (-) (r2h a) (r2h b)

  negate x = h2r $ negate (r2h x)

instance Finite Fr where
  type Order Fr = BLS12_381_Scalar

instance Scale Fr Rust_BLS12_381_G1_Point where
  scale scalar point = unsafePerformIO runScale
    where
      runScale :: IO Rust_BLS12_381_G1_Point
      runScale = do
        dl <- dlopen libPath [RTLD_NOW]
        scalePtr <- dlsym dl "rust_wrapper_scale"
        let !scalef = mkFunScale $ castFunPtr scalePtr

        out <- callocForeignPtrBytes @CChar pointSize
        withForeignPtr (rawData $ rawPoint point) $ \pointPtr -> do
          withForeignPtr (rawData $ rawScalar scalar) $ \scalarPtr -> do
            withForeignPtr out $ \outPtr -> do
              scalef
                (castPtr pointPtr) pointSize
                (castPtr scalarPtr) scalarSize
                pointSize (castPtr outPtr)

        dlclose dl

        return $ RPoint $ RData out

instance CyclicGroup Rust_BLS12_381_G1_Point where
  type ScalarFieldOf Rust_BLS12_381_G1_Point = Fr

  pointGen = h2r pointGen

instance Eq Rust_BLS12_381_G1_Point where
  (==) a b = r2h a == r2h b

instance Arbitrary Rust_BLS12_381_G1_Point where
  arbitrary = h2r <$> arbitrary

instance Show Rust_BLS12_381_G1_Point where
  show = show . r2h

------------------------------------ BLS12-381 G2 ------------------------------------

instance Scale Natural Rust_BLS12_381_G2_Point where
  scale a b = h2r $ scale a (r2h b)

instance Scale Integer Rust_BLS12_381_G2_Point where
  scale a b = h2r $ scale a (r2h b)

instance AdditiveSemigroup Rust_BLS12_381_G2_Point where
  a + b = h2r $ (+) (r2h a) (r2h b)

instance AdditiveMonoid Rust_BLS12_381_G2_Point where
  zero = h2r zero

instance AdditiveGroup Rust_BLS12_381_G2_Point where

  (-) a b = h2r $ (-) (r2h a) (r2h b)

  negate x = h2r $ negate (r2h x)

instance Scale Fr Rust_BLS12_381_G2_Point where
  scale s p = h2r $ scale (r2h s) (r2h p)

instance CyclicGroup Rust_BLS12_381_G2_Point where
  type ScalarFieldOf Rust_BLS12_381_G2_Point = Fr

  pointGen = h2r pointGen

instance Eq Rust_BLS12_381_G2_Point where
  (==) a b = r2h a == r2h b

instance Arbitrary Rust_BLS12_381_G2_Point where
  arbitrary = h2r <$> arbitrary

instance Show Rust_BLS12_381_G2_Point where
  show = show . r2h

-- PolyVec

instance UnivariateRingPolyVec Fr (RustPolyVec Fr) where

  (.*.) :: forall size . (KnownNat size) => RustPolyVec Fr size -> RustPolyVec Fr size -> RustPolyVec Fr size
  l .*. r = unsafePerformIO runHMul
    where
      runHMul :: IO (RustPolyVec Fr size)
      runHMul = do
        dl <- dlopen libPath [RTLD_NOW]
        hmulPtr <- dlsym dl "rust_wrapper_hmul"
        let !hmulf = mkFunHMul $ castFunPtr hmulPtr

        let valueSize = (fromInteger $ naturalToInteger $ value @size)
        out <- callocForeignPtrBytes @CChar (scalarSize P.* valueSize)

        withForeignPtr (rawData $ rawPoly l) $ \lPtr -> do
          withForeignPtr (rawData $ rawPoly r) $ \rPtr -> do
            withForeignPtr out $ \outPtr -> do
              hmulf
                (castPtr lPtr) (valueSize P.* scalarSize)
                (castPtr rPtr) (valueSize P.* scalarSize)
                (valueSize P.* scalarSize) (castPtr outPtr)

        dlclose dl

        return $ RustPV (RData out)

  (.*) = flip (*.)

  (*.) :: forall size . (KnownNat size) => Fr -> RustPolyVec Fr size -> RustPolyVec Fr size
  (*.) a pv = unsafePerformIO runScalarMul
    where
      runScalarMul :: IO (RustPolyVec Fr size)
      runScalarMul = do
        dl <- dlopen libPath [RTLD_NOW]
        scalarMulPtr <- dlsym dl "rust_wrapper_scalar_mul"
        let !scalarMulf = mkFunScalarMul $ castFunPtr scalarMulPtr

        let valueSize = (fromInteger $ naturalToInteger $ value @size)
        out <- callocForeignPtrBytes @CChar (scalarSize P.* valueSize)

        withForeignPtr (rawData $ rawScalar a) $ \aPtr -> do
          withForeignPtr (rawData $ rawPoly pv) $ \pvPtr -> do
            withForeignPtr out $ \outPtr -> do
              scalarMulf
                (castPtr aPtr) scalarSize
                (castPtr pvPtr) (valueSize P.* scalarSize)
                (valueSize P.* scalarSize) (castPtr outPtr)

        dlclose dl

        return $ RustPV (RData out)

  (.+) = flip (+.)

  (+.) :: forall size . (KnownNat size) => Fr -> RustPolyVec Fr size -> RustPolyVec Fr size
  (+.) a pv = unsafePerformIO runScalarAdd
    where
      runScalarAdd :: IO (RustPolyVec Fr size)
      runScalarAdd = do
        dl <- dlopen libPath [RTLD_NOW]
        scalarAddPtr <- dlsym dl "rust_wrapper_scalar_add"
        let !scalarAddf = mkFunScalarAdd $ castFunPtr scalarAddPtr

        let valueSize = (fromInteger $ naturalToInteger $ value @size)
        out <- callocForeignPtrBytes @CChar (scalarSize P.* valueSize)

        withForeignPtr (rawData $ rawScalar a) $ \aPtr -> do
          withForeignPtr (rawData $ rawPoly pv) $ \pvPtr -> do
            withForeignPtr out $ \outPtr -> do
              scalarAddf
                (castPtr aPtr) scalarSize
                (castPtr pvPtr) (valueSize P.* scalarSize)
                (valueSize P.* scalarSize) (castPtr outPtr)

        dlclose dl

        return $ RustPV (RData out)

  toPolyVec :: forall size . (KnownNat size) => V.Vector Fr -> RustPolyVec Fr size
  toPolyVec a = h2r $ toPolyVec (r2h <$> a)

  fromPolyVec :: forall size . (KnownNat size) => RustPolyVec Fr size -> V.Vector Fr
  fromPolyVec a = h2r <$> (fromPolyVec $ r2h a)

  poly2vec :: forall poly size . (KnownNat size, UnivariateRingPolynomial Fr poly) => poly -> RustPolyVec Fr size
  poly2vec = toPolyVec . fromPoly

  vec2poly :: forall poly size . (KnownNat size, UnivariateRingPolynomial Fr poly) => RustPolyVec Fr size -> poly
  vec2poly = toPoly . fromPolyVec

  -- p(x) = a0
  polyVecConstant :: forall size . (KnownNat size) => Fr -> RustPolyVec Fr size
  polyVecConstant a0 = h2r $ polyVecConstant (r2h a0)

  -- p(x) = a1 * x + a0
  polyVecLinear :: forall size . (KnownNat size) => Fr -> Fr -> RustPolyVec Fr size
  polyVecLinear a1 a0 = h2r $ polyVecLinear (r2h a1) (r2h a0)

  -- p(x) = a2 * x^2 + a1 * x + a0
  polyVecQuadratic :: forall size . (KnownNat size) => Fr -> Fr -> Fr -> RustPolyVec Fr size
  polyVecQuadratic a2 a1 a0 = h2r $ polyVecQuadratic (r2h a2) (r2h a1) (r2h a0)

  evalPolyVec :: forall size . (KnownNat size) => RustPolyVec Fr size -> Fr -> Fr
  evalPolyVec pv x = h2r $ evalPolyVec (r2h pv) (r2h x)

instance UnivariateFieldPolyVec Fr (RustPolyVec Fr) where

  (./.) :: forall size . (KnownNat size) => RustPolyVec Fr size -> RustPolyVec Fr size -> RustPolyVec Fr size
  l ./. r = h2r $ ((./.) (r2h l) (r2h r) :: PolyVec EC.Fr size)

  polyVecZero :: forall size . (KnownNat size) => Natural -> RustPolyVec Fr size
  polyVecZero n = h2r $ polyVecZero n

  polyVecLagrange :: forall size . (KnownNat size) => Natural -> Natural -> Fr -> RustPolyVec Fr size
  polyVecLagrange n i omega = h2r $ polyVecLagrange n i (r2h omega)

  polyVecInLagrangeBasis :: forall n size . (KnownNat n, KnownNat size) => Fr -> RustPolyVec Fr n -> RustPolyVec Fr size
  polyVecInLagrangeBasis omega pv = h2r $ polyVecInLagrangeBasis (r2h omega) (r2h pv)

  polyVecGrandProduct :: forall size . (KnownNat size) => RustPolyVec Fr size -> RustPolyVec Fr size -> RustPolyVec Fr size -> Fr -> Fr -> RustPolyVec Fr size
  polyVecGrandProduct a b sigma beta gamma = h2r $ polyVecGrandProduct (r2h a) (r2h b) (r2h sigma) (r2h beta) (r2h gamma)

  polyVecDiv :: forall size . (KnownNat size) => RustPolyVec Fr size -> RustPolyVec Fr size -> RustPolyVec Fr size
  polyVecDiv l r = unsafePerformIO runFFT
    where
      runFFT :: IO (RustPolyVec Fr size)
      runFFT = do
          dl <- dlopen libPath [RTLD_NOW]
          fftPtr <- dlsym dl "rust_wrapper_div_fft"
          let !fft = mkFunDivFFT $ castFunPtr fftPtr

          let valueSize = (fromInteger $ naturalToInteger $ value @size)
          out <- callocForeignPtrBytes @CChar (scalarSize P.* (fromInteger $ naturalToInteger $ value @size))

          withForeignPtr (rawData $ rawPoly l) $ \lPtr -> do
            withForeignPtr (rawData $ rawPoly r) $ \rPtr -> do
              withForeignPtr out $ \outPtr -> do
                fft
                  (castPtr lPtr) (valueSize P.* scalarSize)
                  (castPtr rPtr) (valueSize P.* scalarSize)
                  (valueSize P.* scalarSize) (castPtr outPtr)

          dlclose dl

          return $ RustPV (RData out)

  castPolyVec :: forall size size' . (KnownNat size, KnownNat size') => RustPolyVec Fr size -> RustPolyVec Fr size'
  castPolyVec pv = h2r $ castPolyVec (r2h pv)

instance (KnownNat size) => Scale Natural (RustPolyVec Fr size) where
    scale c pv = h2r $ (scale c (r2h pv) :: PolyVec EC.Fr size)

instance (KnownNat size) => Scale Integer (RustPolyVec Fr size) where
    scale c pv = h2r $ (scale c (r2h pv) :: PolyVec EC.Fr size)

instance (KnownNat size) => FromConstant Natural (RustPolyVec Fr size) where
    fromConstant n = h2r $ fromConstant n

instance (KnownNat size) => FromConstant Integer (RustPolyVec Fr size) where
    fromConstant n = h2r $ fromConstant n

instance (KnownNat size) => AdditiveSemigroup (RustPolyVec Fr size) where
    l + r = h2r $ (+) (r2h l) (r2h r)

instance (KnownNat size) => AdditiveMonoid (RustPolyVec Fr size) where
    zero = h2r $ (zero :: PolyVec EC.Fr size)

instance (KnownNat size) => AdditiveGroup (RustPolyVec Fr size) where
    negate pv = h2r $ negate (r2h pv)

instance (KnownNat size) => Exponent (RustPolyVec Fr size) Natural where
    pv ^ n = h2r $ (^) (r2h pv) n

instance {-# OVERLAPPING #-} (KnownNat size) => Scale (RustPolyVec Fr size) (RustPolyVec Fr size)

-- TODO (Issue #18): check for overflow
instance (KnownNat size) => MultiplicativeSemigroup (RustPolyVec Fr size) where
    (*) l r = unsafePerformIO runFFT
      where
        runFFT :: IO (RustPolyVec Fr size)
        runFFT = do
            dl <- dlopen libPath [RTLD_NOW]
            fftPtr <- dlsym dl "rust_wrapper_mul_fft"
            let !fft = mkFunDivFFT $ castFunPtr fftPtr

            let valueSize = (fromInteger $ naturalToInteger $ value @size)
            out <- callocForeignPtrBytes @CChar (scalarSize P.* (fromInteger $ naturalToInteger $ value @size))

            withForeignPtr (rawData $ rawPoly l) $ \lPtr -> do
              withForeignPtr (rawData $ rawPoly r) $ \rPtr -> do
                withForeignPtr out $ \outPtr -> do
                  fft
                    (castPtr lPtr) (valueSize P.* scalarSize)
                    (castPtr rPtr) (valueSize P.* scalarSize)
                    (valueSize P.* scalarSize) (castPtr outPtr)

            dlclose dl

            return $ RustPV (RData out)


instance (KnownNat size) => MultiplicativeMonoid (RustPolyVec Fr size) where
    one = h2r one

instance (KnownNat size) => Semiring (RustPolyVec Fr size)

instance (KnownNat size) => Ring (RustPolyVec Fr size)

instance (KnownNat size) => Arbitrary (RustPolyVec Fr size) where
    arbitrary = h2r <$> arbitrary

-- TODO: avoid unnecessary casting from Vector to Ptr
instance
  ( KnownNat size
  ) => Bilinear (V.Vector Rust_BLS12_381_G1_Point)
                (RustPolyVec Fr size)
                Rust_BLS12_381_G1_Point where
    bilinear points scalars = unsafePerformIO runMSM
      where
        runMSM :: IO Rust_BLS12_381_G1_Point
        runMSM = do
            dl <- dlopen libPath [RTLD_NOW]
            msmPtr <- dlsym dl "rust_wrapper_msm"
            let !msmf = mkFunMSM $ castFunPtr msmPtr
            let valueSize = min (fromInteger $ naturalToInteger $ value @size) (V.length points)

            out <- callocForeignPtrBytes @CChar pointSize

            pointPtr <- callocBytes @BLS12_381_G1_Point (valueSize P.* pointSize)
            pokeArray pointPtr (V.toList (r2h <$> points))

            withForeignPtr (rawData $ rawPoly scalars) $ \scalarPtr -> do
              withForeignPtr out $ \outPtr -> do
                msmf
                  (castPtr pointPtr) (valueSize P.* pointSize)
                  (castPtr scalarPtr) (valueSize P.* scalarSize)
                  pointSize (castPtr outPtr)

            free pointPtr
            dlclose dl

            return $ RPoint $ RData out
