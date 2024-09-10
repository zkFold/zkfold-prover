use ark_bls12_381::{Fr as ScalarField, G1Affine};
use ark_ff::BigInteger;
use ark_msm::utils::generate_msm_inputs;
use ark_serialize::CanonicalSerialize;
use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use rust_wrapper::msm::rust_wrapper_multi_scalar_multiplication_without_serialization;

fn criterion_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("msm");
    unsafe {
        for size in 10..=14 {
            let (point_vec, scalar_vec) = generate_msm_inputs::<G1Affine>(1 << size);

            let points_bytes_len = G1Affine::identity().uncompressed_size() * (1 << size);
            let scalars_bytes_len = std::mem::size_of::<ScalarField>() * (1 << size);

            let points_ptr = libc::malloc(points_bytes_len) as *mut libc::c_char;
            let scalars_ptr = libc::malloc(scalars_bytes_len) as *mut libc::c_char;

            let points_bytes_vec: Vec<u8> = point_vec
                .iter()
                .flat_map(|a| {
                    let mut vec = Vec::new();
                    let _ = a.serialize_uncompressed(&mut vec);
                    let point_size = 96;
                    vec[0..(point_size >> 1)].reverse();
                    vec[(point_size >> 1)..point_size].reverse();
                    vec
                })
                .collect();

            let scalars_bytes_vec: Vec<u8> = scalar_vec
                .iter()
                .flat_map(BigInteger::to_bytes_le)
                .collect();

            std::ptr::copy(
                points_bytes_vec.as_ptr(),
                points_ptr as *mut u8,
                points_bytes_len,
            );
            std::ptr::copy(
                scalars_bytes_vec.as_ptr(),
                scalars_ptr as *mut u8,
                scalars_bytes_len,
            );

            let out_len = G1Affine::identity().uncompressed_size();
            let out: Vec<u8> = vec![0; out_len];

            group.bench_with_input(BenchmarkId::new("ArkMSM", size), &size, |b, _size| {
                b.iter(|| {
                    rust_wrapper_multi_scalar_multiplication_without_serialization(
                        points_ptr,
                        points_bytes_len,
                        scalars_ptr,
                        scalars_bytes_len,
                        out_len,
                        out.as_ptr() as *mut libc::c_char,
                    );
                })
            });
            libc::free(points_ptr as *mut libc::c_void);
            libc::free(scalars_ptr as *mut libc::c_void);
        }
    }
    group.finish();
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
