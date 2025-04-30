use ark_bls12_381::{Fr as ScalarField, G1Affine as GAffine};
use ark_ff::PrimeField;
use ark_serialize::{CanonicalDeserialize, CanonicalSerialize, SerializationError};
use num_bigint::BigUint;

pub struct Buffer<T>(pub Vec<T>);

pub fn deserialize_vector<T>(
    vector: &[u8],
    object_size: usize,
    deserialize: fn(&[u8]) -> Result<T, SerializationError>,
) -> Buffer<T> {
    Buffer(
        vector
            .chunks_exact(object_size)
            .flat_map(deserialize)
            .collect(),
    )
}

pub fn deserialize_vector_scalar_field(buffer: &[u8]) -> Vec<ScalarField> {
    deserialize_vector(buffer, std::mem::size_of::<ScalarField>(), pack_scalar).0
}

pub fn deserialize_vector_points(buffer: &[u8]) -> Vec<GAffine> {
    deserialize_vector(buffer, GAffine::identity().uncompressed_size(), pack_point).0
}

pub fn fix_point_vector(vector: &mut [u8]) {
    let len = vector.len();
    vector[0..len >> 1].reverse();
    vector[(len >> 1)..len].reverse();
}

pub fn pack_point(bytes: &[u8]) -> Result<GAffine, SerializationError> {
    let mut bytes: Vec<u8> = bytes.to_vec();
    fix_point_vector(&mut bytes);
    GAffine::deserialize_uncompressed_unchecked(&*bytes)
}

pub fn unpack_point(r: GAffine) -> Vec<u8> {
    let mut res = Vec::new();
    r.serialize_uncompressed(&mut res).unwrap();
    fix_point_vector(&mut res);
    res
}

pub fn pack_scalar(bytes: &[u8]) -> Result<ScalarField, SerializationError> {
    Ok(PrimeField::from_le_bytes_mod_order(bytes))
}

pub fn unpack_scalar(scalar: ScalarField) -> Vec<u8> {
    let mut v = BigUint::from(scalar.into_bigint()).to_bytes_le();
    v.resize(std::mem::size_of::<ScalarField>(), 0);
    v
}
