{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module RustFunctions
    ( rustMultiScalarMultiplicationWithoutSerialization
    , rustMulFft
    , RustCore
    ) where

import qualified Data.ByteString                             as BS
import           Data.Maybe                                  (fromJust)
import qualified Data.Vector                                 as V
import           Foreign
import           Foreign.C.String
import           GHC.Base
import           GHC.IO                                      (unsafePerformIO)
import           GHC.Natural                                 (Natural)
import           GHC.Num.Integer                             (integerToInt#)
import           GHC.Num.Natural                             (naturalFromAddr, naturalToAddr)
import           GHC.Ptr                                     (Ptr (..))
import           GHC.TypeNats                                (KnownNat)
import           Prelude                                     hiding (sum)
import           System.Posix.DynamicLinker

import           ZkFold.Base.Algebra.Basic.Field
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381
import           ZkFold.Base.Algebra.EllipticCurve.Class
import           ZkFold.Base.Algebra.Polynomials.Univariate  (fromPolyVec)
import           ZkFold.Base.Data.ByteString
import           ZkFold.Base.Protocol.NonInteractiveProof    (CoreFunction (..), msm)

libPath :: FilePath
libPath = "./lib.so"

type FunFFT =
    CString -> Int -> CString -> Int -> Int -> CString -> IO ()

foreign import ccall "dynamic"
    mkFunFFT :: FunPtr FunFFT -> FunFFT

type FunMSM =
    CString -> Int -> CString -> Int -> Int -> CString -> IO ()

foreign import ccall "dynamic"
    mkFunMSM :: FunPtr FunMSM -> FunMSM

rustMultiScalarMultiplicationWithoutSerialization
    :: forall
        (a :: Type)
        (b :: Natural)
    .   (ScalarField a ~ Zp b
        , Binary (Point a)
        , Storable (ScalarField a)
        , Storable (Point a)
        )
    => [Point a]
    -> [ScalarField a]
    -> Point a
rustMultiScalarMultiplicationWithoutSerialization points scalars = unsafePerformIO runMSM
    where

        pointSize = sizeOf (undefined :: Point a)
        scalarSize = sizeOf (undefined :: ScalarField a)

        pointsByteLength = pointSize * length points
        scalarsByteLength = scalarSize * length scalars


        runMSM :: IO (Point a)
        runMSM = do
            dl <- dlopen libPath [RTLD_NOW]
            msmPtr <- dlsym dl "rust_wrapper_multi_scalar_multiplication_without_serialization"
            let !msmf = mkFunMSM $ castFunPtr msmPtr

            ptrScalars <- callocBytes @(ScalarField a) scalarsByteLength
            ptrPoints <- callocBytes @(Point a) pointsByteLength

            pokeArray ptrScalars scalars
            pokeArray ptrPoints points

            out <- mallocBytes pointSize

            !_ <- msmf
                        (castPtr ptrPoints) pointsByteLength
                        (castPtr ptrScalars) scalarsByteLength
                        pointSize out

            dlclose dl

            res <- BS.packCStringLen (out, pointSize)

            free ptrScalars
            free ptrPoints
            free out

            return $ fromJust $ fromByteString @(Point a) res


data RustCore

peekZpLE :: KnownNat a => Int -> Ptr (Zp a) -> IO (Zp a)
peekZpLE size ptr = do
    let !(Ptr addr) = ptr
    toZp . toInteger <$> naturalFromAddr (int2Word# (integerToInt# $ toInteger size)) addr 0#

pokeZpLE :: Ptr (Zp a) -> Zp a -> IO ()
pokeZpLE ptr p = do
    let !(Ptr addr) = ptr
    !_ <- naturalToAddr (fromZp p) addr 0#
    return ()


instance Storable Fr where
  sizeOf :: Fr -> Int
  sizeOf _ = 32

  alignment :: Fr -> Int
  alignment _ = 8

  peek :: Ptr Fr -> IO Fr
  peek = peekZpLE (sizeOf @Fr undefined)

  poke :: Ptr Fr -> Fr -> IO ()
  poke = pokeZpLE

instance Storable Fq where

  sizeOf :: Fq -> Int
  sizeOf    _ = 48

  alignment :: Fq -> Int
  alignment _ = 8

  peek :: Ptr Fq -> IO Fq
  peek = peekZpLE (sizeOf @Fq undefined)

  poke :: Ptr Fq -> Fq -> IO ()
  poke = pokeZpLE

infByteStringRepr :: [Word8]
infByteStringRepr = replicate 47 0 <> (bit 6 : replicate 48 0)

instance Storable (Point BLS12_381_G1) where

  sizeOf :: Point BLS12_381_G1 -> Int
  sizeOf _ = 96

  alignment :: Point BLS12_381_G1 -> Int
  alignment _ = alignment @Fq undefined

  peek :: Ptr (Point BLS12_381_G1) -> IO (Point BLS12_381_G1)
  peek ptr = do
    a <- BS.packCStringLen (castPtr ptr, sizeOf @(Point BLS12_381_G1) undefined)
    if BS.pack infByteStringRepr == a
    then return Inf
    else do
        x <- peek @Fq (castPtr ptr)
        y <- peek @Fq (ptr `plusPtr` sizeOf @Fq undefined)
        return $ Point x y

  poke :: Ptr (Point BLS12_381_G1) -> Point BLS12_381_G1 -> IO ()
  poke ptr Inf = pokeArray (castPtr ptr) infByteStringRepr
  poke ptr (Point x y) = do
    poke (castPtr ptr) x
    poke (castPtr ptr `plusPtr` sizeOf @Fq undefined) y

instance CoreFunction BLS12_381_G1 RustCore where
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


rustMulFft :: forall f . Storable f => V.Vector f -> V.Vector f -> V.Vector f
rustMulFft l r = if lByteLength * rByteLength == 0 then V.empty else unsafePerformIO runFFT
    where
        scalarSize = sizeOf (undefined :: f)

        lByteLength = scalarSize * V.length l
        rByteLength = scalarSize * V.length r

        runFFT :: IO (V.Vector f)
        runFFT = do
            dl <- dlopen libPath [RTLD_NOW]
            fftPtr <- dlsym dl "rust_wrapper_mul_fft"
            let !fft = mkFunFFT $ castFunPtr fftPtr

            ptrL <- callocBytes @f lByteLength
            ptrR <- callocBytes @f rByteLength

            pokeArray ptrL (V.toList l)
            pokeArray ptrR (V.toList r)

            let outLen = lByteLength + rByteLength - scalarSize
            out <- callocBytes outLen

            !_ <- fft
                        (castPtr ptrL) lByteLength
                        (castPtr ptrR) rByteLength
                        outLen out
            dlclose dl

            V.fromList <$> peekArray @f (V.length l + V.length r - 1) (castPtr out)
