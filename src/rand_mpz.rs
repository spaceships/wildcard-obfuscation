use gmp::mpz::{Mpz, ProbabPrimeResult};
use rand::Rng;

pub fn rand_mpz_prime<R: Rng>(rng: &mut R, n: usize) -> Mpz {
    let r = rand_mpz_range(rng, &Mpz::from(2).pow(n as u32), &Mpz::from(2).pow(n as u32 + 1));
    let p = r.nextprime();
    match p.probab_prime(100000) {
        ProbabPrimeResult::Prime => p,
        ProbabPrimeResult::ProbablyPrime => p,
        ProbabPrimeResult::NotPrime => rand_mpz_prime(rng, n),
    }
}

pub fn rand_mpz<R: Rng>(rng: &mut R, n: usize) -> Mpz {
    let bs: Vec<u8> = (0..n/8+1).map(|i| {
        if i==n/8 {
            rng.gen::<u8>() % ((n%8) as u8)
        } else {
            rng.gen()
        }
    }).collect();
    Mpz::from(bs.as_slice())
}

pub fn rand_mpz_range<R: Rng>(rng: &mut R, start: &Mpz, end: &Mpz) -> Mpz {
    debug_assert!(start < end);
    let mut val = rand_mpz(rng, end.bit_length());
    while &val < start {
        val = rand_mpz(rng, end.bit_length());
    }
    val % end
}

pub fn rand_mpz_mod<R: Rng>(rng: &mut R, q: &Mpz) -> Mpz {
    rand_mpz_range(rng, &Mpz::from(1), q)
}

pub fn rand_mpz_mod_vec<R: Rng>(rng: &mut R, q: &Mpz, n: usize) -> Vec<Mpz> {
    (0..n).map(|_| rand_mpz_mod(rng, q)).collect()
}

