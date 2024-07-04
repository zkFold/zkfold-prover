use std::io::Write;
use std::marker::PhantomData;

use num_bigint::BigUint;

use haskell_ffi::error::Result;
use haskell_ffi::to_haskell::marshall_to_haskell_var;
use haskell_ffi::from_haskell::marshall_from_haskell_var;
use haskell_ffi::{FromHaskell, ToHaskell};

use ark_ff::PrimeField;
use ark_ff::biginteger::BigInt;
use ark_ec::{AffineRepr, VariableBaseMSM};
use ark_test_curves::bls12_381::{G1Projective as G, G1Affine as GAffine, Fr as ScalarField};

pub enum RW {}

/// cbindgen:ignore
pub const RW: PhantomData<RW> = PhantomData;

// TODO: deserialize from BigUint
impl FromHaskell<RW> for ScalarField {
    fn from_haskell(buf: &mut &[u8], tag: PhantomData<RW>) -> Result<Self> {
        let x = <[u64; 4]>::from_haskell(buf, tag)?;
        let f = PrimeField::from_bigint(BigInt::new(x)).unwrap();
        Ok(f)
    }
}

impl ToHaskell<RW> for ScalarField {
    fn to_haskell<W: Write>(&self, writer: &mut W, tag: PhantomData<RW>) -> Result<()> {
        // self.into_bigint().0.to_haskell(writer, tag)
        BigUint::from(self.into_bigint()).to_bytes_le().to_haskell(writer, tag)
    }
}

// TODO: deserialize from BigUint
impl FromHaskell<RW> for GAffine {
    fn from_haskell(buf: &mut &[u8], tag: PhantomData<RW>) -> Result<Self> {
        let (x, y) = <([u64; 6], [u64; 6])>::from_haskell(buf, tag)?;
        Ok(GAffine::new(BigInt::new(x).into(), BigInt::new(y).into()))
    }
}

// TODO: serialize to BigUint
impl ToHaskell<RW> for GAffine {
    fn to_haskell<W: Write>(&self, writer: &mut W, tag: PhantomData<RW>) -> Result<()> {
        let x = self.x().unwrap().into_bigint().0;
        let y = self.y().unwrap().into_bigint().0;
        (x, y).to_haskell(writer, tag)?;
        Ok(())
    }
}

pub fn scalar_mult(a: GAffine, b: GAffine, s1: ScalarField, s2: ScalarField) -> GAffine {
    G::msm(&[a, b], &[s1, s2]).unwrap().into()
}

#[no_mangle]
pub extern "C" fn rust_wrapper_add(x: u64, y: u64) -> u64 {
    x + y
}

#[no_mangle]
pub extern "C" fn rust_wrapper_scalar_mult
(
    a_var: *const u8,
    a_len: usize,
    b_var: *const u8,
    b_len: usize,
    s1_var: *const u8,
    s1_len: usize,
    s2_var: *const u8,
    s2_len: usize,
    out: *mut u8,
    out_len: &mut usize,
) {
    let a: GAffine = marshall_from_haskell_var(a_var, a_len, RW);
    let b: GAffine = marshall_from_haskell_var(b_var, b_len, RW);
    let s1: ScalarField = marshall_from_haskell_var(s1_var, s1_len, RW);
    let s2: ScalarField = marshall_from_haskell_var(s2_var, s2_len, RW);
    let r = scalar_mult(a, b, s1, s2);
    marshall_to_haskell_var(&r, out, out_len, RW);
}
