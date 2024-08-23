module Functions where

#include "rust_wrapper.h"
import Prelude
import Data.ByteString (ByteString)
import Foreign.Rust.Marshall.Variable

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
