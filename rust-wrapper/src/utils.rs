use ark_bls12_381::{Fr as ScalarField, G1Affine as GAffine};
use ark_ff::PrimeField;
use ark_serialize::{CanonicalDeserialize, CanonicalSerialize, SerializationError};

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
        GAffine::deserialize_uncompressed_unchecked(&*bytes)
    })
    .0
}
