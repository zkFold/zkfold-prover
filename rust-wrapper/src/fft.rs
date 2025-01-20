use ark_bls12_381::Fr as ScalarField;
use ark_ff::PrimeField;
use ark_poly::univariate::DensePolynomial;
use ark_poly::DenseUVPolynomial;
use core::slice;
use libc;
use num_bigint::BigUint;
use std::ops::Div;
use std::ops::Mul;
use std::ops::Sub;

use crate::utils::deserialize_vector_scalar_field;

pub fn mul_fft(l: &[u8], r: &[u8]) -> Vec<u8> {
    let l = DensePolynomial::from_coefficients_vec(deserialize_vector_scalar_field(l));
    let r = DensePolynomial::from_coefficients_vec(deserialize_vector_scalar_field(r));

    l.mul(&r)
        .to_vec()
        .iter()
        .flat_map(|x| {
            let mut v = BigUint::from(x.into_bigint()).to_bytes_le();
            v.resize(std::mem::size_of::<ScalarField>(), 0);
            v
        })
        .collect()
}

pub fn div_fft(l: &[u8], r: &[u8]) -> Vec<u8> {
    let l = DensePolynomial::from_coefficients_vec(deserialize_vector_scalar_field(l));
    let r = DensePolynomial::from_coefficients_vec(deserialize_vector_scalar_field(r));

    let quo = l.div(&r);
    let rem = l.sub(&quo.mul(&r));

    let mut quo_vec: Vec<u8> = quo
        .to_vec()
        .iter()
        .flat_map(|x| {
            let mut v = BigUint::from(x.into_bigint()).to_bytes_le();
            v.resize(std::mem::size_of::<ScalarField>(), 0);
            v
        })
        .collect();

    let mut rem_vec: Vec<u8> = rem
        .to_vec()
        .iter()
        .flat_map(|x| {
            let mut v = BigUint::from(x.into_bigint()).to_bytes_le();
            v.resize(std::mem::size_of::<ScalarField>(), 0);
            v
        })
        .collect();

    rem_vec.resize(std::mem::size_of::<ScalarField>() * r.len(), 0);
    quo_vec.append(&mut rem_vec);

    quo_vec
}

///
/// # Safety
/// The caller must ensure that valid pointers and sizes are passed.
/// .
#[no_mangle]
pub unsafe extern "C" fn rust_wrapper_div_fft(
    l_var: *const libc::c_char,
    l_len: usize,
    r_var: *const libc::c_char,
    r_len: usize,
    _out_len: usize,
    out: *mut libc::c_char,
) {
    let l = slice::from_raw_parts(l_var as *const u8, l_len);
    let r = slice::from_raw_parts(r_var as *const u8, r_len);

    let res = div_fft(l, r);

    std::ptr::copy(res.as_ptr(), out as *mut u8, res.len());
}

///
/// # Safety
/// The caller must ensure that valid pointers and sizes are passed.
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
    let l = slice::from_raw_parts(l_var as *const u8, l_len);
    let r = slice::from_raw_parts(r_var as *const u8, r_len);

    let res = mul_fft(l, r);

    std::ptr::copy(res.as_ptr(), out as *mut u8, out_len);
}
