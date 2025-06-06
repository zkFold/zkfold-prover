{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}

module Poly where

import           Control.DeepSeq                      (NFData (..))
import           Conversion
import qualified Data.Vector                          as V
import           Foreign
import           GHC.Base
import           GHC.Generics                         (Generic)
import           GHC.IO                               (unsafePerformIO)
import           Prelude                              hiding (drop, length, product, replicate, sum, take, (/), (^))
import           Types

import           ZkFold.Algebra.Number                (KnownNat, Natural, value)
import           ZkFold.Algebra.Polynomial.Univariate (PolyVec, UnivariateRingPolyVec (..))

newtype RustPolyVec a (size :: Natural) = RustPV { rawPoly :: RustData }
    deriving (Generic)

instance NFData (RustPolyVec a size) where
    rnf _ = ()

instance
    ( Storable h
    , UnivariateRingPolyVec h (PolyVec h)
    , RustHaskell r h
    , KnownNat size
    ) => RustHaskell (RustPolyVec r size) (PolyVec h size) where

    h2r pv = unsafePerformIO $ do
        fptr <- callocForeignPtrBytes ((sizeOf (undefined :: h)) * (fromIntegral $ value @size))
        withForeignPtr fptr $ \ptr -> do
            pokeArray (castPtr ptr) (V.toList (fromPolyVec pv))
            return $ RustPV (RData fptr)

    r2h (RustPV rdata) = unsafePerformIO $
        withForeignPtr (rawData rdata) $ \ptr -> do
            let valueSize = (fromIntegral $ value @size)

            l <- peekArray valueSize (castPtr $ ptr :: Ptr h)
            return $ toPolyVec @h @(PolyVec h) $ V.fromList l
