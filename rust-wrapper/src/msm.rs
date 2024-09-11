use std::slice;

use ark_bls12_381::{Fr as ScalarField, G1Affine as GAffine, G1Projective as G};
use ark_ec::VariableBaseMSM as BaselineVariableBaseMSM;
use ark_ff::PrimeField;
use ark_msm::msm::VariableBaseMSM;
use ark_serialize::CanonicalSerialize;
use ark_std::log2;
use haskell_ffi::{from_haskell::marshall_from_haskell_var, to_haskell::marshall_to_haskell_var};
use utils::{Buffer, RW};

use crate::utils::{self, deserialize_vector_points, deserialize_vector_scalar_field};

///
/// # Safety
/// The caller must make sure that correctly serialized scalars are passed.
/// Correct serialization/deserialization is described in ToHaskell/FromHaskell trait.
/// .
#[no_mangle]
pub unsafe extern "C" fn rust_wrapper_multi_scalar_multiplication(
    points_var: *const u8,
    points_len: usize,
    scalars_var: *const u8,
    scalars_len: usize,
    out: *mut u8,
    out_len: &mut usize,
) {
    let scalars =
        marshall_from_haskell_var::<RW, Buffer<ScalarField>>(scalars_var, scalars_len, RW).0;
    let points = marshall_from_haskell_var::<RW, Buffer<GAffine>>(points_var, points_len, RW).0;

    let r: GAffine = G::msm(&points, &scalars).unwrap().into();

    marshall_to_haskell_var(&r, out, out_len, RW);
}
// This is inner function from arkmsm crate, derived from benchmark results. May not be optimal for all configurations
// https://github.com/snarkify/arkmsm/blob/main/src/msm.rs
const fn get_opt_window_size(k: u32) -> u32 {
    match k {
        0..=9 => 8,
        10..=12 => 10,
        13..=14 => 12,
        15..=19 => 13,
        20..=22 => 15,
        23.. => 16,
    }
}

pub fn multi_scalar_multiplication_without_serialization(
    scalar_buffer: &[u8],
    point_buffer: &[u8],
) -> Vec<u8> {
    let scalars: Vec<_> = deserialize_vector_scalar_field(scalar_buffer)
        .iter()
        .map(|i| i.into_bigint())
        .collect();

    let points = deserialize_vector_points(point_buffer);

    let opt_window_size = get_opt_window_size(log2(points.len()));
    let r: GAffine = VariableBaseMSM::multi_scalar_mul_custom(
        &points,
        &scalars,
        opt_window_size,
        2048,
        256,
        false,
    )
    .into();

    let mut res = Vec::new();
    r.serialize_uncompressed(&mut res).unwrap();
    res
}

///
/// # Safety
/// The caller must ensure that valid pointers and sizes are passed.
/// .
#[no_mangle]
pub unsafe extern "C" fn rust_wrapper_multi_scalar_multiplication_without_serialization(
    points_var: *const libc::c_char,
    points_len: usize,
    scalars_var: *const libc::c_char,
    scalars_len: usize,
    out_len: usize,
    out: *mut libc::c_char,
) {
    let scalar_buffer = slice::from_raw_parts(scalars_var as *const u8, scalars_len);
    let point_buffer = slice::from_raw_parts(points_var as *const u8, points_len);

    let res = multi_scalar_multiplication_without_serialization(scalar_buffer, point_buffer);
    
    std::ptr::copy(res.as_ptr(), out as *mut u8, out_len);
}
