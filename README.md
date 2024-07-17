# zkFold Prover
Optimized ZK provers


```rust
:dep ark-ec
:dep ark-std
:dep ark-test-curves
:dep num-bigint

use ark_test_curves::bls12-381;

use num_bigint::BigUint;

use ark_ec::scalar_mul::variable_base::VariableBaseMSM;

let a: G1Affine = G1Affine::rand(&mut ark_std::test_rng());

>> (241521825108085326152546143885075698096242717769365851673777146265962830024243722276743782280873588223519920495647, 3560035591319159527750795167323375962990500740637199979279756662028071590138066280420930564248685402405154128130764)

let b: G1Affine = G1Affine::rand(&mut ark_std::test_rng());

>> (241521825108085326152546143885075698096242717769365851673777146265962830024243722276743782280873588223519920495647, 3560035591319159527750795167323375962990500740637199979279756662028071590138066280420930564248685402405154128130764)

let s1: Fr = Fr::rand(&mut ark_std::test_rng());

>> BigInt([12346421629811869064, 10832332258257352915, 17999185152888039383, 7443919619818212425])

BigUint::from(s1)

>> 46726240763639862128214388288720131204625575015731614850157206947646262134152

let s2: Fr = Fr::rand(&mut ark_std::test_rng());

>> BigInt([12346421629811869064, 10832332258257352915, 17999185152888039383, 7443919619818212425])

let r = G1Projective::msm(&[a, b], &[s1, s2]).unwrap();

>> (1171139267616720494075625646925889629053973114062983274112141117287962730244572401315926224670976668703533933610938, 2597500562880127794764544429111412706443897715838599281188622858902365963355368167613499961260759309831461122771787, 157755297696137070468630334947167064094148264806415657673627072440504500291622319980244878217128135252257702585508)
```

```haskell
let p1 = Point @BLS12_381_G1 241521825108085326152546143885075698096242717769365851673777146265962830024243722276743782280873588223519920495647 3560035591319159527750795167323375962990500740637199979279756662028071590138066280420930564248685402405154128130764

let p2 = Point @BLS12_381_G1 241521825108085326152546143885075698096242717769365851673777146265962830024243722276743782280873588223519920495647 3560035591319159527750795167323375962990500740637199979279756662028071590138066280420930564248685402405154128130764

let s1 = 46726240763639862128214388288720131204625575015731614850157206947646262134152

let s2 = 46726240763639862128214388288720131204625575015731614850157206947646262134152

 withBorshVarBuffer (rustWrapperScalarMult p1 p2 s1 s2)
```