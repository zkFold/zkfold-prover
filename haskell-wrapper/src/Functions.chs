module Functions where

#include "rust_wrapper.h"
import Codec.Borsh
import Data.ByteString (ByteString)
import Data.FixedSizeArray
import Foreign.Rust.Marshall.Fixed
import Foreign.Rust.Marshall.Variable
import Foreign.Rust.Serialisation.Raw

{# fun unsafe rust_wrapper_scalar_sum as rustWrapperScalarSum
     { toBorshVar* `ByteString'&
     , toBorshVar* `ByteString'&
     , getVarBuffer `Buffer ByteString'&
     }
  -> `()'
#}

{# fun unsafe rust_wrapper_multi_scalar_multiplication as rustWrapperMultiScalarMultiplication
     { toBorshVar* `ByteString'&
     , toBorshVar* `ByteString'&
     , getVarBuffer `Buffer ByteString'&
     }
  -> `()'
#}
