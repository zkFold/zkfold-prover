{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Main where

import           Control.DeepSeq                             (force)
import           Control.Exception                           (evaluate)
import           Control.Monad                               (forM_, replicateM)
import qualified Data.Vector                                 as V
import           Foreign
import           Prelude                                     hiding (sum, (*), (+), (-), (/), (^))
import qualified Prelude                                     as P
import           RustFunctions                               (rustMulFft)
import           System.Random                               (randomIO)
import           Test.Tasty.Bench

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Field
import           ZkFold.Base.Algebra.Basic.Number            (Prime)
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381
import           ZkFold.Base.Algebra.EllipticCurve.Class
import           ZkFold.Base.Algebra.Polynomials.Univariate

-- | Generate random polynomials of given size
--
polynomials :: forall a. Prime a => Int -> IO (Poly (Zp a), Poly (Zp a))
polynomials size = do
    coeffs1 <- replicateM size (toZp @a <$> randomIO)
    coeffs2 <- replicateM size (toZp @a <$> randomIO)
    evaluatedCoeffs1 <- evaluate . force . V.fromList $ coeffs1
    evaluatedCoeffs2 <- evaluate . force . V.fromList $ coeffs2
    pure (toPoly evaluatedCoeffs1, toPoly evaluatedCoeffs2)

sizes :: [Int]
sizes = (((4 :: Int) P.^) <$> [2..5 :: Int]) <> ((( 2 :: Int) P.^) <$> [11..12 :: Int])

ops :: forall a . (Eq a, Field a, Storable a) => [(String, Poly a -> Poly a -> Poly a)]
ops = [ ("DFT multiplication", mulPolyDft)
      , ("Adaptive multiplication", (*))
      , ("Karatsuba multiplication", mulPolyKaratsuba)
      , ("Vector multiplication", mulPoly)
      , ("Naive multiplication", mulPolyNaive)
      , ("Rust FFT", \x y -> toPoly (rustMulFft @a (fromPoly x) (fromPoly y)))
      ]

benchOps :: Prime a => Int -> [(String, Poly (Zp a) -> Poly (Zp a) -> Poly (Zp a))] -> Benchmark
benchOps size testOps = env (polynomials size) $ \ ~(p1, p2) ->
    bgroup ("Multiplying polynomials of size " <> show size) $
            flip fmap testOps $ \(desc, op) -> bench desc $ nf (uncurry op) (p1, p2)


main :: IO ()
main = do
  forM_ sizes $ \s -> do
      (p1, p2) <- polynomials @BLS12_381_Scalar s
      putStrLn $ "Size " <> show s
      let ref = p1 `mulPolyNaive` p2
      putStrLn $ "Karatsuba\t" <> show (ref == p1 `mulPolyKaratsuba` p2)
      putStrLn $ "Vector\t\t"  <> show (ref == p1 `mulPoly` p2)
      putStrLn $ "DFT\t\t"     <> show (ref == p1 `mulPolyDft` p2)
      putStrLn $ "RustFFT\t\t" <> show (ref == toPoly (rustMulFft @(ScalarField BLS12_381_G1) (fromPoly p1) (fromPoly p2)))
  defaultMain
      [ bgroup "Field with roots of unity"           $ flip fmap sizes $ \s -> benchOps @BLS12_381_Scalar s ops
      , bgroup "Field without roots of unity"        $ flip fmap sizes $ \s -> benchOps @BLS12_381_Base s $ tail ops
      ]