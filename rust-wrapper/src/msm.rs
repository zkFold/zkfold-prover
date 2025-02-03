use std::slice;

use ark_bls12_381::Fr as ScalarField;
use ark_bls12_381::G1Affine as GAffine;
use ark_ec::CurveGroup;
use ark_ff::PrimeField;
use ark_msm::msm::VariableBaseMSM;
use ark_serialize::{CanonicalDeserialize, CanonicalSerialize};
use ark_std::log2;

use crate::utils::{deserialize_vector_points, deserialize_vector_scalar_field};

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

pub fn msm(scalar_buffer: &[u8], point_buffer: &[u8]) -> Vec<u8> {
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

pub fn mul(scalar_buffer: &[u8], point_buffer: &[u8]) -> Vec<u8> {
    let scalar: ScalarField = PrimeField::from_le_bytes_mod_order(scalar_buffer);

    let mut bytes: Vec<u8> = point_buffer.to_vec();
    let points_size = bytes.len();
    bytes[0..(points_size >> 1)].reverse();
    bytes[(points_size >> 1)..points_size].reverse();
    let point = GAffine::deserialize_uncompressed_unchecked(&*bytes).unwrap();

    let r: GAffine = (point * scalar).into_affine();
    print!("Rust mul");
    let mut res = Vec::new();
    r.serialize_uncompressed(&mut res).unwrap();
    res
}

///
/// # Safety
/// The caller must ensure that valid pointers and sizes are passed.
/// .
#[no_mangle]
pub unsafe extern "C" fn rust_wrapper_msm(
    points_var: *const libc::c_char,
    points_len: usize,
    scalars_var: *const libc::c_char,
    scalars_len: usize,
    out_len: usize,
    out: *mut libc::c_char,
) {
    let scalar_buffer = slice::from_raw_parts(scalars_var as *const u8, scalars_len);
    let point_buffer = slice::from_raw_parts(points_var as *const u8, points_len);

    let res = msm(scalar_buffer, point_buffer);

    std::ptr::copy(res.as_ptr(), out as *mut u8, out_len);
}

///
/// # Safety
/// The caller must ensure that valid pointers and sizes are passed.
/// .
#[no_mangle]
pub unsafe extern "C" fn rust_wrapper_mul(
    points_var: *const libc::c_char,
    points_len: usize,
    scalars_var: *const libc::c_char,
    scalars_len: usize,
    out_len: usize,
    out: *mut libc::c_char,
) {
    let scalar_buffer = slice::from_raw_parts(scalars_var as *const u8, scalars_len);
    let point_buffer = slice::from_raw_parts(points_var as *const u8, points_len);

    let res = mul(scalar_buffer, point_buffer);

    std::ptr::copy(res.as_ptr(), out as *mut u8, out_len);
}
