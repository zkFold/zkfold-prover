{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Main where

import           Control.DeepSeq                             (force)
import           Control.Exception                           (evaluate)
import           Control.Monad                               (replicateM)
import           Data.Tuple.Extra
import qualified Data.Vector                                 as V
import           Foreign
import           Prelude                                     hiding (sum, (*), (+), (-), (/), (^))
import qualified Prelude                                     as P
import           RustFunctions                               (rustDivFft)
import           System.Random                               (randomIO)
import           Test.Tasty.Bench

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Field
import           ZkFold.Base.Algebra.Basic.Number            (Prime)
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381
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
sizes = ((2 :: Int) P.^) <$> [10 .. 14 :: Int]

ops :: forall a . (Eq a, Field a, Storable a) => [(String, Poly a -> Poly a -> (Poly a, Poly a))]
ops = [ ("Haskell division", qr)
      , ("Rust division", \x y -> both toPoly (rustDivFft @a (fromPoly x) (fromPoly y)))
      ]

benchOps :: Prime a => Int -> [(String, Poly (Zp a) -> Poly (Zp a) -> (Poly (Zp a), Poly (Zp a))    )] -> Benchmark
benchOps size testOps = env (polynomials size) $ \ ~(p1, p2) ->
    bgroup ("Division polynomials of size " <> show size) $
            flip fmap testOps $ \(desc, op) -> bench desc $ nf (uncurry op) (p1, p2)

main :: IO ()
main = do
  defaultMain
      [ bgroup "Field without roots of unity"        $ flip fmap sizes $ \s -> benchOps @BLS12_381_Base s ops
      , bgroup "Field with roots of unity"           $ flip fmap sizes $ \s -> benchOps @BLS12_381_Scalar s ops
      ]
