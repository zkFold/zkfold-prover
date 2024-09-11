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
    vector: &[u8],
    object_size: usize,
    deserialize: fn(&[u8]) -> Result<T>,
) -> Buffer<T> {
    Buffer(
        vector
            .chunks_exact(object_size)
            .flat_map(deserialize)
            .collect(),
    )
}

pub fn deserialize_vector_scalar_field(buffer: &[u8]) -> Vec<ScalarField> {
    deserialize_vector(buffer, std::mem::size_of::<ScalarField>(), |x| {
        Ok(PrimeField::from_le_bytes_mod_order(x))
    })
    .0
}

pub fn deserialize_vector_points(buffer: &[u8]) -> Vec<GAffine> {
    deserialize_vector(buffer, GAffine::identity().uncompressed_size(), |bytes| {
        let mut bytes: Vec<u8> = bytes.to_vec();
        let points_size = bytes.len();
        bytes[0..(points_size >> 1)].reverse();
        bytes[(points_size >> 1)..points_size].reverse();
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
        let buffer = <Vec<u8>>::from_haskell(buf, tag)?;
        let res = deserialize_vector(&buffer, std::mem::size_of::<ScalarField>(), |bytes| {
            Ok(PrimeField::from_le_bytes_mod_order(bytes))
        });
        Ok(res)
    }
}

impl FromHaskell<RW> for Buffer<GAffine> {
    fn from_haskell(buf: &mut &[u8], tag: PhantomData<RW>) -> Result<Self> {
        let buffer = <Vec<u8>>::from_haskell(buf, tag)?;
        let res = deserialize_vector(&buffer, GAffine::identity().uncompressed_size(), |bytes| {
            Ok(GAffine::deserialize_uncompressed_unchecked(bytes)?)
        });
        Ok(res)
    }
}
