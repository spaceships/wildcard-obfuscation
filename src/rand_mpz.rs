use gmp::mpz::{Mpz, ProbabPrimeResult};
use rand::Rng;

pub fn rand_mpz<R: Rng>(rng: &mut R, n: usize) -> Mpz {
    let bs: Vec<u8> = (0..n/8).map(|i| {
        if i==n/8 {
            rng.gen::<u8>() % (1<<((n%8) as u8))
        } else {
            rng.gen()
        }
    }).collect();
    Mpz::from(bs.as_slice())
}

pub fn rand_mpz_range<R: Rng>(rng: &mut R, start: &Mpz, end: &Mpz) -> Mpz {
    debug_assert!(start < end);
    let val = rand_mpz(rng, end.bit_length());
    (val + start) % end
}

pub fn rand_mpz_mod<R: Rng>(rng: &mut R, q: &Mpz) -> Mpz {
    rand_mpz_range(rng, &Mpz::from(1), q)
}

pub fn rand_mpz_mod_vec<R: Rng>(rng: &mut R, q: &Mpz, n: usize) -> Vec<Mpz> {
    (0..n).map(|_| rand_mpz_mod(rng, q)).collect()
}

pub fn rand_mpz_prime<R: Rng>(rng: &mut R, n: usize) -> Mpz {
    let r = rand_mpz_range(rng, &Mpz::from(2).pow(n as u32), &Mpz::from(2).pow(n as u32 + 1));
    r.nextprime()
}

pub fn is_prime(x: &Mpz) -> bool {
    match x.probab_prime(128) {
        ProbabPrimeResult::Prime => true,
        ProbabPrimeResult::ProbablyPrime => true,
        ProbabPrimeResult::NotPrime => false,
    }
}

pub fn mpz_strong_prime(n: usize) -> (Mpz, Mpz) {
    let mut q = Mpz::from(1) << n;
    let mut p = Mpz::from(2) * &q + Mpz::from(1);
    while !is_prime(&p) || !is_prime(&q) {
        q = q.nextprime();
        p = Mpz::from(2) * &q + Mpz::from(1);
    }
    (p, q)
}
