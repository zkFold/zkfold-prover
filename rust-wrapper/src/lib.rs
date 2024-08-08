use std::io::Write;
use std::marker::PhantomData;

use num_bigint::BigUint;

use haskell_ffi::error::Result;
use haskell_ffi::to_haskell::marshall_to_haskell_var;
use haskell_ffi::from_haskell::{marshall_from_haskell_var};
use haskell_ffi::{FromHaskell, ToHaskell};

use ark_ff::PrimeField;
use ark_ec::{VariableBaseMSM};
use ark_test_curves::bls12_381::{G1Projective as G, G1Affine as GAffine, Fr as ScalarField};
use ark_serialize::{CanonicalDeserialize, CanonicalSerialize};

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
        BigUint::from(self.into_bigint()).to_bytes_le().to_haskell(writer, tag)
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

fn deserialize_vector<T>(vector: Vec<u8>, object_size: usize, deserialize: fn(&[u8]) -> T) -> Buffer<T> {
    Buffer(
        vector
        .chunks_exact(object_size)
        .map(|chunk| { deserialize(chunk) })
        .collect()
    )
}

impl FromHaskell<RW> for Buffer<ScalarField> {
    fn from_haskell(buf: &mut &[u8], tag: PhantomData<RW>) -> Result<Self> {
        let buffer = <Vec<u8>>::from_haskell(buf, tag)?;
        let res = deserialize_vector(buffer, 32, PrimeField::from_le_bytes_mod_order);
        Ok(res)
    }
}

impl FromHaskell<RW> for Buffer<GAffine> {
    fn from_haskell(buf: &mut &[u8], tag: PhantomData<RW>) -> Result<Self> {
        let buffer = <Vec<u8>>::from_haskell(buf, tag)?;
        let res = deserialize_vector(buffer, 96, 
            |bytes| -> GAffine { GAffine::deserialize_uncompressed(&*bytes).unwrap() });
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
pub extern "C" fn rust_wrapper_multi_scalar_multiplication
(
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
