{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module RustFunctions where

import           Foreign
import           Foreign.C.String
import           Prelude

type FunMulFFT =
    CString -> Int -> CString -> Int -> Int -> CString -> IO ()

foreign import ccall "dynamic"
    mkFunMulFFT :: FunPtr FunMulFFT -> FunMulFFT

type FunDivFFT =
    CString -> Int -> CString -> Int -> Int -> CString -> IO ()

foreign import ccall "dynamic"
    mkFunDivFFT :: FunPtr FunDivFFT -> FunDivFFT

type FunMSM =
    CString -> Int -> CString -> Int -> Int -> CString -> IO ()

foreign import ccall "dynamic"
    mkFunMSM :: FunPtr FunMSM -> FunMSM

type FunScale =
    CString -> Int -> CString -> Int -> Int -> CString -> IO ()

foreign import ccall "dynamic"
    mkFunScale :: FunPtr FunScale -> FunScale

type FunSum =
    CString -> Int -> CString -> Int -> Int -> CString -> IO ()

foreign import ccall "dynamic"
    mkFunSum :: FunPtr FunSum -> FunSum

type FunMul =
    CString -> Int -> CString -> Int -> Int -> CString -> IO ()

foreign import ccall "dynamic"
    mkFunMul :: FunPtr FunMul -> FunMul

type FunHMul =
    CString -> Int -> CString -> Int -> Int -> CString -> IO ()

foreign import ccall "dynamic"
    mkFunHMul :: FunPtr FunHMul -> FunHMul

type FunScalarMul =
    CString -> Int -> CString -> Int -> Int -> CString -> IO ()

foreign import ccall "dynamic"
    mkFunScalarMul :: FunPtr FunScalarMul -> FunScalarMul

type FunScalarAdd =
    CString -> Int -> CString -> Int -> Int -> CString -> IO ()

foreign import ccall "dynamic"
    mkFunScalarAdd :: FunPtr FunScalarAdd -> FunScalarAdd
