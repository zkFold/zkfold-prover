{-# OPTIONS_GHC -Wno-orphans #-}

module Class where

import           Control.DeepSeq
import           Foreign
import           Foreign.C.Types
import           Prelude
import           ZkFold.Base.Algebra.EllipticCurve.Class

libPath :: FilePath
libPath = "libs/librust_wrapper.so"

callocForeignPtrBytes :: Int -> IO (ForeignPtr a)
callocForeignPtrBytes n = do { p <- callocBytes n; newForeignPtr finalizerFree p }

newtype Scalar curve s = Scalar { rawScalar :: s }
    deriving (NFData)

type FCString = ForeignPtr CChar

instance NFData FCString where
    rnf _ = ()

newtype RustData = RData { rawData :: FCString }
    deriving (NFData)

-- Scalar BLS

type Fr = Scalar "Rust BLS12-381-G1 Fr" RustData

type Fq = Scalar "Rust BLS12-381-G1 Fq" RustData

-- Point BLS

type Rust_BLS12_381_G1_Point = Weierstrass "Rust BLS12-381-G1" RustData

type Rust_BLS12_381_G2_Point = Weierstrass "Rust BLS12-381-G2" RustData

-- Compressed point BLS

type Rust_BLS12_381_G1_CompressedPoint = Weierstrass "Rust BLS12-381-G1 Compressed" RustData

type Rust_BLS12_381_G2_CompressedPoint = Weierstrass "Rust BLS12-381-G2 Compressed" RustData
