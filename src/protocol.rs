use rand_mpz::*;

use gmp::mpz::Mpz;
use rand;
use std;
use num::rational::Ratio;

pub struct WildcardObfuscation {
    p: Mpz,             // prime modulus
    h: Vec<[Mpz; 2]>,   // h_ij encodings
}

impl WildcardObfuscation {

    // pub fn encode<R: Rng>(rng: &mut R, g: &Mpz, p: &Mpz, f: &Vec<Mpz>, pat: &Vec<usize>) -> Vec<[Mpz; 2]> {
    pub fn encode(pat: &str, secparam: usize) -> Self {
        let n = pat.len();

        let ref mut rng = rand::thread_rng();

        // we precompute some primes for benchmarking
        let p = match secparam {
            16 => Mpz::from_str_radix("99347", 10).unwrap(),
            128 => Mpz::from_str_radix("384502684745947809683888132747790323231", 10).unwrap(),
            _ => {
                eprint!("generating {}-bit prime...", secparam);
                let prime = rand_mpz_prime(rng, secparam);
                eprintln!("{}", prime);
                prime
            }
        };

        let g = Mpz::from(2);

        // let ref f = rand_mpz_mod_vec(rng, &p, n-1);
        let ref f = vec![Mpz::from(1); n-1];
        // eprintln!("p = {}", p);
        // eprintln!("F = {:?}", f);

        // create the h_ij encodings
        let mut h = Vec::with_capacity(pat.len());
        for (i, elem) in pat.chars().enumerate() {
            h.push([Mpz::from(0), Mpz::from(0)]);

            for &j in [0,1].iter() {
                let j_as_char = std::char::from_digit(j as u32, 10).unwrap();
                let point: i32 = 2*(i as i32 + 1) + j as i32;

                if elem == j_as_char || elem == '*' {
                    let y = poly_eval(f, &Mpz::from(point));
                    h[i][j] = g.powm(&y, &p);
                } else {
                    // h[i][j] = rand_mpz_mod(rng, &p);
                    h[i][j] = Mpz::from(0);
                }
            }
        }

        eprintln!("h={:?}", h);

        WildcardObfuscation { p, h }
    }

    pub fn eval(&self, inp: &str) -> usize {
        assert_eq!(inp.len(), self.h.len(), "error: expected {}-bit input, but got {} bits!", self.h.len(), inp.len());
        let x: Vec<usize> = inp.chars().map(|c| c.to_digit(2).expect("binary digit") as usize).collect();
        // eprintln!("x={:?}", x);

        let mut t = Mpz::from(1);
        for i in 0..inp.len() {
            let c = lagrange_coef(i, &x);
            // eprintln!("c({},x) = {}", i, c);
            let val = self.h[i][x[i]].powm(&Mpz::from(c), &self.p);
            // eprintln!("val={}", val);
            t *= val;
            t %= &self.p;
        }
        eprintln!("t={}", t);

        (t == Mpz::from(1)) as usize
    }
}

fn lagrange_coef(i: usize, x: &[usize]) -> i32 {
    let mut prod = Ratio::from_integer(1);
    for j in 0..x.len() {
        if i == j { continue }
        let pi = ( 2*(i + 1) + x[i] ) as i32;
        let pj = ( 2*(j + 1) + x[j] ) as i32;
        prod *= Ratio::new(-pj, pi - pj);
    }
    prod.to_integer()
}

fn poly_eval(coefs: &Vec<Mpz>, x: &Mpz) -> Mpz {
    let mut y = Mpz::from(0);
    for i in 0..coefs.len() {
        if x == &Mpz::from(0) { continue }
        y += &coefs[i] * x.pow(i as u32 + 1)
    }
    y
}

