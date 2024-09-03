module Functions where

#include "rust_wrapper.h"
import Foreign.C.String
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

{# fun pure unsafe rust_wrapper_multi_scalar_multiplication_without_serialization as ^ 
  { `CString'
  , `Int' 

  , `CString'
  , `Int' 
  
  , `Int' 
  , `CString'
  } 
  -> `()'
#}
