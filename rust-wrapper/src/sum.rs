use ark_bls12_381::Fr as ScalarField;
use haskell_ffi::{from_haskell::marshall_from_haskell_var, to_haskell::marshall_to_haskell_var};
use utils::RW;

use crate::utils;

#[no_mangle]
pub extern "C" fn rust_wrapper_scalar_sum(
    a_var: *const u8,
    a_len: usize,
    b_var: *const u8,
    b_len: usize,
    out: *mut u8,
    out_len: &mut usize,
) {
    let a: ScalarField = marshall_from_haskell_var(a_var, a_len, RW);
    let b: ScalarField = marshall_from_haskell_var(b_var, b_len, RW);
    let r: ScalarField = a + b;
    marshall_to_haskell_var(&r, out, out_len, RW);
}
