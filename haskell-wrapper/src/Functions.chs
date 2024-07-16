module Functions (rustWrapperAdd) where

#include "rust_wrapper.h"
import Data.Int
import Data.Word
import Data.Functor.Contravariant
import Codec.Borsh
import Data.FixedSizeArray
import Foreign.Rust.Marshall.Fixed
import Foreign.Rust.Marshall.Variable
import Foreign.Rust.Serialisation.Raw

import ZkFold.Base.Algebra.Basic.Class
import ZkFold.Base.Algebra.Basic.Field
import ZkFold.Base.Algebra.EllipticCurve.Class
import ZkFold.Base.Algebra.EllipticCurve.BLS12_381

-- pub type Fq = Fp384<MontBackend<FqConfig, 6>>;
-- pub type Fr = Fp256<MontBackend<FrConfig, 4>>;

instance BorshSize Fr where
   type StaticBorshSize Fr = HasVariableSize

instance FromBorsh Fr where
   decodeBorsh = toZp . toInteger <$> decodeBorsh @Int64

instance ToBorsh Fr where
   encodeBorsh = fromInteger . toInteger . fromZp >$< encodeBorsh @Int64

instance BorshSize Fq where
   type StaticBorshSize Fq = HasVariableSize

instance FromBorsh Fq where
   decodeBorsh = toZp . toInteger <$> decodeBorsh @Int64

instance ToBorsh Fq where
   encodeBorsh = fromInteger . toInteger . fromZp >$< encodeBorsh @Int64

instance BorshSize (Point BLS12_381_G1) where
   type StaticBorshSize (Point BLS12_381_G1) = HasVariableSize

instance ToBorsh (Point BLS12_381_G1) where
   encodeBorsh = (\(Point x y) -> (x, y)) >$< encodeBorsh @(Fq, Fq)

{# fun pure unsafe rust_wrapper_add as rustWrapperAdd
     { `Word64'
     , `Word64'
     }
  -> `Word64'
#}

{# fun unsafe rust_wrapper_scalar_mult as rustWrapperScalarMult
     { toBorshVar* `Point BLS12_381_G1'&
     , toBorshVar* `Point BLS12_381_G1'&
     , toBorshVar* `ScalarField BLS12_381_G1'&
     , toBorshVar* `ScalarField BLS12_381_G1'&
     , getVarBuffer `Buffer (Point BLS12_381_G1)'&
     }
  -> `()'
#}
