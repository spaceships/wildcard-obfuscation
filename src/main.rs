extern crate gmp;
extern crate rand;

use gmp::mpz::{Mpz, ProbabPrimeResult};
use rand::Rng;

fn main() {
    let ref mut rng = rand::thread_rng();

    let n = 2;

    // let ref p = rand_mpz_prime(rng, 128);
    let ref p = Mpz::from_str_radix("384502684745947809683888132747790323231", 10).unwrap(); // 128 bit prime
    let ref g = Mpz::from(2);


    let ref mut f = rand_mpz_mod_vec(rng, p, n);
    f[0] = Mpz::from(0);

    let ref h = encode(rng, g, p, f, &vec![0,0]);

    let result = eval(p, h, &vec![0,0]);

    println!("{}", result);
}

fn encode<R: Rng>(rng: &mut R, g: &Mpz, p: &Mpz, f: &Vec<Mpz>, pat: &Vec<usize>) -> Vec<[Mpz; 2]> {
    let mut h = Vec::with_capacity(pat.len());
    for (i, &elem) in pat.iter().enumerate() {
        h.push([Mpz::from(0), Mpz::from(0)]);
        for j in 0..2 {
            let pt: i32 = 2*(i as i32 + 1) + j as i32;
            if elem == j || elem >= 2 {
                let y = poly_eval(f, &Mpz::from(pt));
                h[i][j] = g.powm(&y, p);
            } else {
                h[i][j] = rand_mpz_mod(rng, &p);
            }
        }
    }
    h
}

fn eval(p: &Mpz, h: &Vec<[Mpz; 2]>, x: &Vec<usize>) -> usize {
    let mut t = Mpz::from(1);
    for i in 0..x.len() {
        let c = lagrange_coef(x, i);
        t *= h[i][x[i]].powm(&c, p);
        t %= p;
    }

    (t == Mpz::from(1)) as usize
}

fn lagrange_coef(x: &[usize], i: usize) -> Mpz {
    let mut prod = Mpz::from(1);
    for j in 0..x.len() {
        if i == j { continue }
        let pi: i32 = 2*(i as i32 + 1) + x[i] as i32;
        let pj: i32 = 2*(j as i32 + 1) + x[j] as i32;
        prod *= Mpz::from(-pj) / Mpz::from(pi - pj);
    }
    prod
}

fn poly_eval(coefs: &Vec<Mpz>, x: &Mpz) -> Mpz {
    let mut y = coefs[0].clone();
    for i in 1..coefs.len() {
        if x == &Mpz::from(0) { continue }
        y += &coefs[i] * x.pow(i as u32)
    }
    y
}

fn rand_mpz_mod_vec<R: Rng>(rng: &mut R, q: &Mpz, n: usize) -> Vec<Mpz> {
    (0..n).map(|_| rand_mpz_mod(rng, q)).collect()
}

#[allow(dead_code)]
fn rand_mpz_prime<R: Rng>(rng: &mut R, n: usize) -> Mpz {
    let r = rand_mpz_range(rng, &Mpz::from(2).pow(n as u32), &Mpz::from(2).pow(n as u32 + 1));
    let p = r.nextprime();
    match p.probab_prime(100000) {
        ProbabPrimeResult::Prime => p,
        ProbabPrimeResult::ProbablyPrime => p,
        ProbabPrimeResult::NotPrime => rand_mpz_prime(rng, n),
    }
}

fn rand_mpz<R: Rng>(rng: &mut R, n: usize) -> Mpz {
    let bs: Vec<u8> = (0..n/8+1).map(|i| {
        if i==n/8 {
            rng.gen::<u8>() % ((n%8) as u8)
        } else {
            rng.gen()
        }
    }).collect();
    Mpz::from(bs.as_slice())
}

fn rand_mpz_range<R: Rng>(rng: &mut R, start: &Mpz, end: &Mpz) -> Mpz {
    debug_assert!(start < end);
    let mut val = rand_mpz(rng, end.bit_length());
    while &val < start {
        val = rand_mpz(rng, end.bit_length());
    }
    val % end
}

fn rand_mpz_mod<R: Rng>(rng: &mut R, q: &Mpz) -> Mpz {
    rand_mpz_range(rng, &Mpz::from(1), q)
}

