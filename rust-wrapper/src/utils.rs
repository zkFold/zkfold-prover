use ark_bls12_381::{Fr as ScalarField, G1Affine as GAffine};
use ark_ff::PrimeField;
use ark_serialize::{CanonicalDeserialize, CanonicalSerialize};
use haskell_ffi::error::Result;
use haskell_ffi::{FromHaskell, ToHaskell};
use num_bigint::BigUint;
use std::io::Write;
use std::marker::PhantomData;

pub enum RW {}

pub const RW: PhantomData<RW> = PhantomData;

pub struct Buffer<T>(pub Vec<T>);

pub fn deserialize_vector<T>(
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

pub fn deserialize_vector_scalar_field(
    scalars_var: *const libc::c_char,
    scalars_len: usize,
) -> Vec<ScalarField> {
    let mut vec: Vec<u8> = vec![0; scalars_len];
    unsafe {
        std::ptr::copy(scalars_var as *const u8, vec.as_mut_ptr(), scalars_len);
    }
    let scalars: Vec<_> = deserialize_vector(&mut vec, 32, |x| -> ScalarField {
        PrimeField::from_le_bytes_mod_order(x)
    })
    .0;
    scalars
}

pub fn deserialize_vector_points(
    points_var: *const libc::c_char,
    points_len: usize,
) -> Vec<GAffine> {
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
    points
}

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
