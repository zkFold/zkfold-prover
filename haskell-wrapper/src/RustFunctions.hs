{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module RustFunctions where

import           Foreign
import           Foreign.C.String
import           Prelude

type FunScalarAdd =
    CString -> Int -> CString -> Int -> Int -> CString -> IO ()

foreign import ccall unsafe
    "rust_wrapper_scalar_add" rsScalarAdd :: FunScalarAdd

type FunMulFFT =
    CString -> Int -> CString -> Int -> Int -> CString -> IO ()

foreign import ccall unsafe
    "rust_wrapper_mul_fft" rsMulFFT :: FunMulFFT

type FunDivFFT =
    CString -> Int -> CString -> Int -> Int -> CString -> IO ()

foreign import ccall unsafe
    "rust_wrapper_div_fft" rsDivFFT :: FunDivFFT

type FunMSM =
    CString -> Int -> CString -> Int -> Int -> CString -> IO ()

foreign import ccall unsafe
    "rust_wrapper_msm" rsMSM :: FunMSM

type FunScale =
    CString -> Int -> CString -> Int -> Int -> CString -> IO ()

foreign import ccall unsafe
    "rust_wrapper_scale" rsScale :: FunScale

type FunSum =
    CString -> Int -> CString -> Int -> Int -> CString -> IO ()

foreign import ccall unsafe
    "rust_wrapper_sum" rsSum :: FunSum

type FunMul =
    CString -> Int -> CString -> Int -> Int -> CString -> IO ()

foreign import ccall unsafe
    "rust_wrapper_mul" rsMul :: FunMul

type FunHMul =
    CString -> Int -> CString -> Int -> Int -> CString -> IO ()

foreign import ccall unsafe
    "rust_wrapper_hmul" rsHMul :: FunHMul

type FunScalarMul =
    CString -> Int -> CString -> Int -> Int -> CString -> IO ()

foreign import ccall unsafe
    "rust_wrapper_scalar_mul" rsScalarMul :: FunScalarMul
