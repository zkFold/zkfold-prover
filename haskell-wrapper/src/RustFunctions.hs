{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module RustFunctions
    ( rustMultiScalarMultiplicationWithoutSerialization
    , rustMulFft
    , rustDivFft
    , rustMulPoint
    , RustCore
    , both
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
import           Prelude                                     hiding (rem, sum)
import           System.Posix.DynamicLinker

import           ZkFold.Base.Algebra.Basic.Class             (AdditiveMonoid (zero))
import           ZkFold.Base.Algebra.Basic.Field
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381
import           ZkFold.Base.Algebra.EllipticCurve.Class
import           ZkFold.Base.Algebra.Polynomials.Univariate  (fromPoly, fromPolyVec, qr, toPoly)
import           ZkFold.Base.Data.ByteString
import           ZkFold.Base.Protocol.NonInteractiveProof    (CoreFunction (..), msm)

libPath :: FilePath
libPath = "libs/librust_wrapper.so"

type FunMulFFT =
    CString -> Int -> CString -> Int -> Int -> CString -> IO ()

type FunDivFFT =
    CString -> Int -> CString -> Int -> Int -> CString -> IO ()

foreign import ccall "dynamic"
    mkFunMulFFT :: FunPtr FunMulFFT -> FunMulFFT

foreign import ccall "dynamic"
    mkFunDivFFT :: FunPtr FunDivFFT -> FunDivFFT

type FunMSM =
    CString -> Int -> CString -> Int -> Int -> CString -> IO ()

foreign import ccall "dynamic"
    mkFunMSM :: FunPtr FunMSM -> FunMSM

type FunMul =
    CString -> Int -> CString -> Int -> Int -> CString -> IO ()

foreign import ccall "dynamic"
    mkFunMul :: FunPtr FunMul -> FunMul


rustMulPoint
    :: forall
        (a :: Type)
        (b :: Natural)
    .   (ScalarField a ~ Zp b
        , Binary (Point a)
        , Storable (ScalarField a)
        , Storable (Point a)
        )
    => Point a
    -> ScalarField a
    -> Point a
rustMulPoint point scalar = unsafePerformIO runMSM
    where

        pointSize = sizeOf (undefined :: Point a)
        scalarSize = sizeOf (undefined :: ScalarField a)

        runMSM :: IO (Point a)
        runMSM = do
            dl <- dlopen libPath [RTLD_NOW]
            mulPtr <- dlsym dl "rust_wrapper_mul"
            let !mulf = mkFunMul $ castFunPtr mulPtr

            ptrScalar <- callocBytes @(ScalarField a) scalarSize
            ptrPoint <- callocBytes @(Point a) pointSize

            poke ptrScalar scalar
            poke ptrPoint point

            out <- mallocBytes pointSize

            !_ <- mulf
                        (castPtr ptrPoint) pointSize
                        (castPtr ptrScalar) scalarSize
                        pointSize out

            dlclose dl

            res <- BS.packCStringLen (out, pointSize)

            free ptrScalar
            free ptrPoint
            free out

            return $ fromJust $ fromByteString @(Point a) res


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
            msmPtr <- dlsym dl "rust_wrapper_msm"
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

instance forall f . (ScalarField f ~ Zp BLS12_381_Scalar, BaseField f ~ Zp BLS12_381_Base, BooleanOf f ~ Bool) =>  Storable (Point f) where

  sizeOf :: Point f -> Int
  sizeOf _ = 96

  alignment :: Point f -> Int
  alignment _ = alignment @Fq undefined

  peek :: Ptr (Point f) -> IO (Point f)
  peek ptr = do
    a <- BS.packCStringLen (castPtr ptr, sizeOf @(Point f) undefined)
    if BS.pack infByteStringRepr == a
    then return $ Point zero zero True
    else do
        x <- peek @Fq (castPtr ptr)
        y <- peek @Fq (ptr `plusPtr` sizeOf @Fq undefined)
        return $ Point x y False

  poke :: Ptr (Point f) -> Point f -> IO ()
  poke ptr (Point _ _ True) = pokeArray (castPtr ptr) infByteStringRepr
  poke ptr (Point x y False) = do
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

    polyMul x y = toPoly (rustMulFft @(ScalarField BLS12_381_G1) (fromPoly x) (fromPoly y))
    polyQr x y = both toPoly $ rustDivFft @(ScalarField BLS12_381_G1) (fromPoly x) (fromPoly y)

both :: (t -> b) -> (t, t) -> (b, b)
both f (x, y) = (f x, f y)

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
            let !fft = mkFunMulFFT $ castFunPtr fftPtr

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
            free ptrL
            free ptrR
            dlclose dl

            !res <- V.fromList <$> peekArray @f (V.length l + V.length r - 1) (castPtr out)

            free out

            return res

-- Should be without leading zeroes
rustDivFft :: forall f . Storable f => V.Vector f -> V.Vector f -> (V.Vector f, V.Vector f)
rustDivFft l r  | rByteLength == 0 = error "Polynomial division by zero"
                | lByteLength < rByteLength = (V.empty, l)
                | otherwise = unsafePerformIO runFFT
    where
        scalarSize = sizeOf (undefined :: f)

        lByteLength = scalarSize * V.length l
        rByteLength = scalarSize * V.length r

        runFFT :: IO (V.Vector f, V.Vector f)
        runFFT = do
            dl <- dlopen libPath [RTLD_NOW]
            fftPtr <- dlsym dl "rust_wrapper_div_fft"
            let !fft = mkFunDivFFT $ castFunPtr fftPtr

            ptrL <- callocBytes @f lByteLength
            ptrR <- callocBytes @f rByteLength

            pokeArray ptrL (V.toList l)
            pokeArray ptrR (V.toList r)

            let outLen = (V.length l + 1) * scalarSize
            out <- callocBytes outLen

            !_ <- fft
                        (castPtr ptrL) lByteLength
                        (castPtr ptrR) rByteLength
                        outLen out

            free ptrL
            free ptrR
            dlclose dl



            !quo <- V.fromList <$> peekArray @f (V.length l - V.length r + 1) (castPtr out)
            !rem <- V.fromList <$> peekArray @f (V.length r) (castPtr out `plusPtr` ((V.length l - V.length r + 1 ) * scalarSize))


            free out

            return (quo, rem)
