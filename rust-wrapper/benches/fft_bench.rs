use ark_bls12_381::Fr as ScalarField;
use ark_ff::{BigInt, BigInteger};
use ark_poly::univariate::DensePolynomial;
use ark_poly::DenseUVPolynomial;
use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use rust_wrapper::fft::rust_wrapper_mul_fft;

fn criterion_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("fft");
    unsafe {
        for size in 10..=14 {
            let degree = 1 << size;

            let mut rng = &mut ark_std::test_rng();

            let l: DensePolynomial<ScalarField> =
                DensePolynomial::<ScalarField>::rand(degree, &mut rng);
            let r: DensePolynomial<ScalarField> =
                DensePolynomial::<ScalarField>::rand(degree, &mut rng);

            let l_bytes_len = std::mem::size_of::<ScalarField>() * (degree + 1);
            let r_bytes_len = std::mem::size_of::<ScalarField>() * (degree + 1);

            let l_ptr = libc::malloc(l_bytes_len) as *mut libc::c_char;
            let r_ptr = libc::malloc(r_bytes_len) as *mut libc::c_char;

            let r_bytes_vec: Vec<u8> = r
                .coeffs
                .iter()
                .flat_map(|i| (BigInt::from(*i)).to_bytes_le())
                .collect();

            let l_bytes_vec: Vec<u8> = l
                .coeffs
                .iter()
                .flat_map(|i| (BigInt::from(*i)).to_bytes_le())
                .collect();

            std::ptr::copy(
                l_bytes_vec.as_ptr(),
                l_ptr as *mut u8,
                l_bytes_len,
            );
            std::ptr::copy(
                r_bytes_vec.as_ptr(),
                r_ptr as *mut u8,
                r_bytes_len,
            );

            let out_len = std::mem::size_of::<ScalarField>() * (2 * degree + 1);
            let out: Vec<u8> = vec![0; out_len];

            group.bench_with_input(
                BenchmarkId::new("FFT Multiplication", degree),
                &degree,
                |b, _size| {
                    b.iter(|| {
                        rust_wrapper_mul_fft(
                            l_ptr,
                            l_bytes_len,
                            r_ptr,
                            r_bytes_len,
                            out_len,
                            out.as_ptr() as *mut libc::c_char,
                        );
                    })
                },
            );
            libc::free(l_ptr as *mut libc::c_void);
            libc::free(r_ptr as *mut libc::c_void);
        }
    }
    group.finish();
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
