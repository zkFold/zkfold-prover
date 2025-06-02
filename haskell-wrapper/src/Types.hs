{-# OPTIONS_GHC -Wno-orphans #-}

module Types where

import           Control.DeepSeq
import           Data.Bool                  as B
import           Foreign
import           Foreign.C.Types
import           GHC.Generics
import           Prelude

import           ZkFold.Control.Conditional
libPath :: FilePath
libPath = "libs/librust_wrapper.so"

callocForeignPtrBytes :: Int -> IO (ForeignPtr a)
callocForeignPtrBytes n = do { p <- callocBytes n; newForeignPtr finalizerFree p }

newtype Scalar curve s = RScalar { rawScalar :: s }
    deriving (NFData, Generic)

newtype Point curve s = RPoint { rawPoint :: s }
    deriving (NFData, Generic)

type FCString = ForeignPtr CChar

instance NFData FCString where
    rnf _ = ()

newtype RustData = RData { rawData :: FCString }
    deriving (NFData, Generic)

instance Conditional Bool RustData where bool = B.bool

-- Scalar BLS

type Fr = Scalar "Rust BLS12-381-G1 Fr" RustData

type Fq = Scalar "Rust BLS12-381-G1 Fq" RustData

-- Point BLS

type Rust_BLS12_381_G1_Point = Point "Rust BLS12-381-G1" RustData

type Rust_BLS12_381_G2_Point = Point "Rust BLS12-381-G2" RustData

-- Compressed point BLS

type Rust_BLS12_381_G1_CompressedPoint = Point "Rust BLS12-381-G1 Compressed" RustData

type Rust_BLS12_381_G2_CompressedPoint = Point "Rust BLS12-381-G2 Compressed" RustData
