use ark_bls12_381::{Fr as ScalarField, G1Affine as GAffine, G1Projective as G};
use ark_ec::VariableBaseMSM as BaselineVariableBaseMSM;
use ark_ff::PrimeField;
use ark_msm::msm::VariableBaseMSM;
use ark_serialize::{CanonicalDeserialize, CanonicalSerialize};
use ark_std::log2;
use haskell_ffi::error::Result;
use haskell_ffi::from_haskell::marshall_from_haskell_var;
use haskell_ffi::to_haskell::marshall_to_haskell_var;
use haskell_ffi::{FromHaskell, ToHaskell};
use libc;
use num_bigint::BigUint;
use std::io::Write;
use std::marker::PhantomData;

pub enum RW {}

pub const RW: PhantomData<RW> = PhantomData;

impl FromHaskell<RW> for ScalarField {
    fn from_haskell(buf: &mut &[u8], tag: PhantomData<RW>) -> Result<Self> {
        let x = <Vec<u8>>::from_haskell(buf, tag)?;
        let s1: ScalarField = PrimeField::from_le_bytes_mod_order(&x);
        Ok(s1)
    }
}

impl ToHaskell<RW> for ScalarField {
    fn to_haskell<W: Write>(&self, writer: &mut W, tag: PhantomData<RW>) -> Result<()> {
        BigUint::from(self.into_bigint())
            .to_bytes_le()
            .to_haskell(writer, tag)
    }
}

impl FromHaskell<RW> for GAffine {
    fn from_haskell(buf: &mut &[u8], tag: PhantomData<RW>) -> Result<Self> {
        let _a = <Vec<u8>>::from_haskell(buf, tag)?;
        let a = GAffine::deserialize_uncompressed(&*_a).unwrap();
        Ok(a)
    }
}

impl ToHaskell<RW> for GAffine {
    fn to_haskell<W: Write>(&self, writer: &mut W, tag: PhantomData<RW>) -> Result<()> {
        let mut res = Vec::new();
        self.serialize_uncompressed(&mut res).unwrap();
        res.to_haskell(writer, tag)
    }
}

struct Buffer<T>(Vec<T>);

fn deserialize_vector<T>(
    vector: &mut Vec<u8>,
    object_size: usize,
    deserialize: fn(&mut [u8]) -> T,
) -> Buffer<T> {
    Buffer(
        vector
            .chunks_exact_mut(object_size)
            .map(|chunk| deserialize(chunk))
            .collect(),
    )
}

impl FromHaskell<RW> for Buffer<ScalarField> {
    fn from_haskell(buf: &mut &[u8], tag: PhantomData<RW>) -> Result<Self> {
        let mut buffer = <Vec<u8>>::from_haskell(buf, tag)?;
        let res = deserialize_vector(&mut buffer, 32, |bytes| -> ScalarField {
            PrimeField::from_le_bytes_mod_order(bytes)
        });
        Ok(res)
    }
}

impl FromHaskell<RW> for Buffer<GAffine> {
    fn from_haskell(buf: &mut &[u8], tag: PhantomData<RW>) -> Result<Self> {
        let mut buffer = <Vec<u8>>::from_haskell(buf, tag)?;
        let res = deserialize_vector(&mut buffer, 96, |bytes| -> GAffine {
            GAffine::deserialize_uncompressed_unchecked(&*bytes).unwrap()
        });
        Ok(res)
    }
}

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

#[no_mangle]
pub extern "C" fn rust_wrapper_multi_scalar_multiplication(
    points_var: *const u8,
    points_len: usize,
    scalars_var: *const u8,
    scalars_len: usize,
    out: *mut u8,
    out_len: &mut usize,
) {
    let scalars: Vec<ScalarField> =
        marshall_from_haskell_var::<RW, Buffer<ScalarField>>(scalars_var, scalars_len, RW).0;
    let points: Vec<GAffine> =
        marshall_from_haskell_var::<RW, Buffer<GAffine>>(points_var, points_len, RW).0;

    let r: GAffine = G::msm(&points, &scalars).unwrap().into();

    marshall_to_haskell_var(&r, out, out_len, RW);
}

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

#[no_mangle]
pub extern "C" fn rust_wrapper_multi_scalar_multiplication_without_serialization(
    points_var: *const libc::c_char,
    points_len: usize,
    scalars_var: *const libc::c_char,
    scalars_len: usize,
    out_len: usize,
    out: *mut libc::c_char,
) {
    let mut vec: Vec<u8> = vec![0; scalars_len];
    unsafe {
        std::ptr::copy(scalars_var as *const u8, vec.as_mut_ptr(), scalars_len);
    }
    let scalars: Vec<_> = deserialize_vector(&mut vec, 32, |x| -> ScalarField {
        PrimeField::from_le_bytes_mod_order(x)
    })
    .0
    .iter()
    .map(|i| {
        return i.into_bigint();
    })
    .collect();

    let mut vec: Vec<u8> = vec![0; points_len];
    unsafe {
        std::ptr::copy(points_var as *const u8, vec.as_mut_ptr(), points_len);
    }
    let points: Vec<GAffine> = deserialize_vector(&mut vec, 96, |bytes| -> GAffine {
        bytes[0..48].reverse();
        bytes[48..96].reverse();
        GAffine::deserialize_uncompressed_unchecked(&*bytes).unwrap()
    })
    .0;

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
    unsafe {
        std::ptr::copy(res.as_ptr() as *const u8, out as *mut u8, out_len);
    }
}
