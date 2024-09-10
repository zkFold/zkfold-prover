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
    vector: &mut [u8],
    object_size: usize,
    deserialize: fn(&mut [u8]) -> Result<T>,
) -> Buffer<T> {
    Buffer(
        vector
            .chunks_exact_mut(object_size)
            .flat_map(deserialize)
            .collect(),
    )
}

pub unsafe fn deserialize_vector_scalar_field(
    scalars_var: *const libc::c_char,
    scalars_len: usize,
) -> Vec<ScalarField> {
    let mut vec: Vec<u8> = vec![0; scalars_len];
    std::ptr::copy(scalars_var as *const u8, vec.as_mut_ptr(), scalars_len);

    deserialize_vector(&mut vec, std::mem::size_of::<ScalarField>(), |x| {
        Ok(PrimeField::from_le_bytes_mod_order(x))
    })
    .0
}

pub unsafe fn deserialize_vector_points(
    points_var: *const libc::c_char,
    points_len: usize,
) -> Vec<GAffine> {
    let mut vec: Vec<u8> = vec![0; points_len];
    std::ptr::copy(points_var as *const u8, vec.as_mut_ptr(), points_len);
    deserialize_vector(&mut vec, GAffine::identity().uncompressed_size(), |bytes| {
        let point_size = bytes.len();
        bytes[0..(point_size >> 1)].reverse();
        bytes[(point_size >> 1)..point_size].reverse();
        Ok(GAffine::deserialize_uncompressed_unchecked(&*bytes)?)
    })
    .0
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
        let a = GAffine::deserialize_uncompressed(&*_a)?;
        Ok(a)
    }
}

impl ToHaskell<RW> for GAffine {
    fn to_haskell<W: Write>(&self, writer: &mut W, tag: PhantomData<RW>) -> Result<()> {
        let mut res = Vec::new();
        self.serialize_uncompressed(&mut res)?;
        res.to_haskell(writer, tag)
    }
}

impl FromHaskell<RW> for Buffer<ScalarField> {
    fn from_haskell(buf: &mut &[u8], tag: PhantomData<RW>) -> Result<Self> {
        let mut buffer = <Vec<u8>>::from_haskell(buf, tag)?;
        let res = deserialize_vector(&mut buffer, std::mem::size_of::<ScalarField>(), |bytes| {
            Ok(PrimeField::from_le_bytes_mod_order(bytes))
        });
        Ok(res)
    }
}

impl FromHaskell<RW> for Buffer<GAffine> {
    fn from_haskell(buf: &mut &[u8], tag: PhantomData<RW>) -> Result<Self> {
        let mut buffer = <Vec<u8>>::from_haskell(buf, tag)?;
        let res = deserialize_vector(
            &mut buffer,
            GAffine::identity().uncompressed_size(),
            |bytes| Ok(GAffine::deserialize_uncompressed_unchecked(&*bytes)?),
        );
        Ok(res)
    }
}
