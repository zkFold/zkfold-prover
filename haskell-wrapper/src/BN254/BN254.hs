{-# OPTIONS_GHC -Wno-orphans #-}

module BN254.BN254 where

import           ZkFold.Base.Algebra.Basic.Field         (Zp)
import           ZkFold.Base.Algebra.Basic.Number        (Prime)
import           ZkFold.Base.Algebra.EllipticCurve.Class (EllipticCurve (..), Point (Inf, Point), pointAdd, pointMul)

type BN254_Scalar = 21888242871839275222246405745257275088548364400416034343698204186575808495617
instance Prime BN254_Scalar

type BN254_Base = 21888242871839275222246405745257275088696311157297823662689037894645226208583
instance Prime BN254_Base

type Fr = Zp BN254_Scalar
type Fp = Zp BN254_Base

data BN254_G1

instance EllipticCurve BN254_G1 where
  type ScalarField BN254_G1 = Fr
  type BaseField BN254_G1 = Fp
  inf = Inf
  gen = Point 1 2
  add = pointAdd
  mul = pointMul
