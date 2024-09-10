use ark_bls12_381::Fr as ScalarField;
use ark_ff::PrimeField;
use ark_poly::univariate::DensePolynomial;
use ark_poly::DenseUVPolynomial;
use libc;
use num_bigint::BigUint;
use std::ops::Mul;

use crate::utils::deserialize_vector_scalar_field;

///
/// # Safety
/// The caller must ensure that valid pointers and sizes are passed. 
/// To find out how data should be represented in memory, you can look at fft_bench.rs
/// .
#[no_mangle]
pub unsafe extern "C" fn rust_wrapper_mul_fft(
    l_var: *const libc::c_char,
    l_len: usize,
    r_var: *const libc::c_char,
    r_len: usize,
    out_len: usize,
    out: *mut libc::c_char,
) {
    let l = DensePolynomial::from_coefficients_vec(deserialize_vector_scalar_field(l_var, l_len));
    let r = DensePolynomial::from_coefficients_vec(deserialize_vector_scalar_field(r_var, r_len));

    let res: Vec<u8> = l
        .mul(&r)
        .to_vec()
        .iter()
        .flat_map(|x| {
            let mut v = BigUint::from(x.into_bigint()).to_bytes_le();
            v.resize(std::mem::size_of::<ScalarField>(), 0);
            v
        })
        .collect();

    std::ptr::copy(res.as_ptr(), out as *mut u8, out_len);
}
