module Functions (rustWrapperAdd) where

#include "rust_wrapper.h"
import Data.Word
import Codec.Borsh
import Data.FixedSizeArray
import Foreign.Rust.Marshall.Fixed
import Foreign.Rust.Marshall.Variable
import Foreign.Rust.Serialisation.Raw

import ZkFold.Base.Algebra.Basic.Class
import ZkFold.Base.Algebra.Basic.Field
import ZkFold.Base.Algebra.EllipticCurve.Class
import ZkFold.Base.Algebra.EllipticCurve.BLS12_381

-- instance BorshSize Fr where
--     type StaticBorshSize Fr = StaticBorshSize
--         (FixedSizeArray 6 Word64, FixedSizeArray 6 Word64)

-- pub type Fq = Fp384<MontBackend<FqConfig, 6>>;
-- pub type Fr = Fp256<MontBackend<FrConfig, 4>>;

-- instance FromBorsh Fr where
--    decodeBorsh = decodeLittleEndian

-- deriving via (AsStruct ) instance (ToBorsh Fr)
-- deriving via (AsStruct Fr) instance (FromBorsh Fr)

instance BorshSize (Point BLS12_381_G1) where
    type StaticBorshSize (Point BLS12_381_G1) = HasVariableSize

-- deriving instance BorshMaxSize (Point BLS12_381_G1)
-- deriving instance ToBorsh (Point BLS12_381_G1)
-- deriving instance FromBorsh (Point BLS12_381_G1)
-- deriving instance IsRaw (Point BLS12_381_G1)

{# fun pure unsafe rust_wrapper_add as rustWrapperAdd
     { `Word64'
     , `Word64'
     }
  -> `Word64'
#}

{# fun unsafe rust_wrapper_scalar_mult as rustWrapperScalarMult
     { toBorshFixed* `Point BLS12_381_G1'&
     , toBorshFixed* `Point BLS12_381_G1'&
     , toBorshFixed* `ScalarField BLS12_381_G1'&
     , toBorshFixed* `ScalarField BLS12_381_G1'&
     , getVarBuffer `Buffer (Point BLS12_381_G1)'&
     }
  -> `()'
#}
