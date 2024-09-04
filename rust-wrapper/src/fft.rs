use ark_ff::PrimeField;
use ark_poly::univariate::DensePolynomial;
use ark_poly::DenseUVPolynomial;
use libc;
use num_bigint::BigUint;
use std::ops::Mul;

use crate::utils::deserialize_vector_scalar_field;

fn pad_end(mut vec: Vec<u8>, target_length: usize) -> Vec<u8> {
    while vec.len() < target_length {
        vec.push(0);
    }
    vec
}

#[no_mangle]
pub extern "C" fn rust_wrapper_mul_fft(
    l_var: *const libc::c_char,
    l_len: usize,
    r_var: *const libc::c_char,
    r_len: usize,
    _out_len: usize,
    out: *mut libc::c_char,
) {
    let l = DensePolynomial::from_coefficients_vec(deserialize_vector_scalar_field(l_var, l_len));
    let r = DensePolynomial::from_coefficients_vec(deserialize_vector_scalar_field(r_var, r_len));

    let res: Vec<Vec<u8>> = l
        .mul(&r)
        .to_vec()
        .iter()
        .map(|x| pad_end(BigUint::from(x.into_bigint()).to_bytes_le(), 32))
        .collect();

    unsafe {
        std::ptr::copy(
            res.join(&[][..]).as_ptr() as *const u8,
            out as *mut u8,
            res.len() * 32,
        );
    }
}
