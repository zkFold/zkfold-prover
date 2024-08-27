{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators       #-}
module Pack
    ( packPoint
    , unpackPoint
    , packScalar
    , unpackScalar
    ) where

import           Data.Binary                             (Word8)
import           Data.Bits                               (Bits (bit, clearBit, testBit))
import qualified Data.ByteString                         as BS
import           Data.Kind                               (Type)
import           Data.Maybe                              (fromJust)
import           GHC.TypeNats                            (KnownNat)
import           Numeric.Natural                         (Natural)
import           Prelude

import           ZkFold.Base.Algebra.Basic.Field         (Zp, fromZp, toZp)
import           ZkFold.Base.Algebra.EllipticCurve.Class (EllipticCurve (BaseField, ScalarField), Point (Inf, Point))
import           ZkFold.Base.Data.ByteString             (LittleEndian (..), fromByteString, toByteString)

padByteStringToLength :: Int -> BS.ByteString -> BS.ByteString
padByteStringToLength n bs = BS.append bs padding
  where
    lenDiff = n - BS.length bs
    padding = BS.replicate lenDiff 0

packNat :: Int -> Natural -> BS.ByteString
packNat n = padByteStringToLength n . toByteString . LittleEndian

unpackNat :: BS.ByteString -> Natural
unpackNat = unLittleEndian . fromJust . fromByteString @LittleEndian

packScalar
    :: forall
        (a :: Type)
        (b :: Natural)
    .   ( ScalarField a ~ Zp b
        , KnownNat b
        )
    => ScalarField a
    -> BS.ByteString
packScalar = toByteString

unpackScalar
    :: forall
        (a :: Type)
        (b :: Natural)
    .   ( ScalarField a ~ Zp b
        , KnownNat b
        )
    => BS.ByteString
    -> ScalarField a
unpackScalar = fromJust . fromByteString

packPoint
    :: forall
        (a :: Type)
        (b :: Natural)
    .   ( BaseField a ~ Zp b
        )
    =>Point a
    -> BS.ByteString
packPoint Inf         = BS.snoc (BS.replicate 95 0) (bit 6)
packPoint (Point x y) = packNat 48 (fromZp x) <> packNat 48 (fromZp y)

unpackPoint
    :: forall
        (a :: Type)
        (b :: Natural)
    .   ( BaseField a ~ Zp b
        , KnownNat b
        )
    => BS.ByteString
    -> Point a
unpackPoint bs = if isInfinity then Inf @a else Point @a x y
  where
    substring :: Int -> Int -> BS.ByteString -> BS.ByteString
    substring start len = BS.take len . BS.drop start

    xs :: BS.ByteString
    xs = substring 0 48 bs

    ys :: BS.ByteString
    ys = substring 48 48 bs

    lastByte :: Word8
    lastByte = BS.last bs

    isInfinity :: Bool
    isInfinity = testBit lastByte 6

    dropFlags :: Word8
    dropFlags = clearBit lastByte 7

    x :: Zp b
    x = toZp $ toInteger $ unpackNat xs

    y :: Zp b
    y = toZp $ toInteger $ unpackNat $ BS.snoc (BS.init ys) dropFlags
