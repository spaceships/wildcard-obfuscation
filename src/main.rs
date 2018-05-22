extern crate gmp;
extern crate rand;

use gmp::mpz::Mpz;
use rand::Rng;

fn main() {
    println!("Hello, world!");

    let ref mut rng = rand::thread_rng();

    let n = 2;

    // let ref p = rand_mpz_prime(rng, 8);
    let ref p = Mpz::from(251);
    let ref g = Mpz::from(2);

    println!("p={}", p);

    let ref mut f = rand_mpz_mod_vec(rng, p, n);
    f[0] = Mpz::from(0);

    let ref h = encode(rng, g, p, f, &vec![0,0]);
    println!("h={:?}", h);

    let result = eval(g, p, h, &vec![0,0]);

    println!("result={}", result);
}

fn encode<R: Rng>(rng: &mut R, g: &Mpz, p: &Mpz, f: &Vec<Mpz>, pat: &Vec<usize>) -> Vec<[Mpz; 2]> {
    let mut h = Vec::with_capacity(pat.len());
    for (i, &elem) in pat.iter().enumerate() {
        h.push([Mpz::from(0), Mpz::from(0)]);
        for j in 0..2 {
            if elem == j || elem >= 2 {
                println!("pat");
                let y = poly_eval(f, &Mpz::from(2*(i as u32 + 1) + j as u32), p);
                // h[i][j] = g.powm(&y, p);
                h[i][j] = y;
            } else {
                // h[i][j] = rand_mpz_mod(rng, &p) + Mpz::from(1);
                h[i][j] = Mpz::from(0);
            }
            println!("i={} j={} h[{}][{}]={}", i, j, i, j, h[i][j]);
        }
    }
    h
}

fn eval(g: &Mpz, p: &Mpz, h: &Vec<[Mpz; 2]>, x: &Vec<usize>) -> bool {
    let mut t = Mpz::from(1);
    for i in 0..x.len() {
        let c = lagrange_coef(x, i, p);
        println!("t={} c={}", t, c);
        // t *= h[i][x[i]].powm(&c, p);
        t += &h[i][x[i]] * c;
        t %= p;
    }

    println!("t={} g={} p={}", t, g, p);
    &t == g
}

fn lagrange_coef(x: &[usize], i: usize, p: &Mpz) -> Mpz {
    let n = x.len();
    let mut prod = Mpz::from(1);
    for j in 0..x.len() {
        if i == j { continue }
        let num = -Mpz::from((2*(j+1) - x[j]) as i32);
        let denom = Mpz::from((2*(i+1) - x[i] - x[j] + 2*(j+1)) as i32);
        let val = (num * denom.invert(p).unwrap()).modulus(p);
        prod *= val;
        prod %= p;
    }
    prod
}

fn poly_eval(coefs: &Vec<Mpz>, x: &Mpz, p: &Mpz) -> Mpz {
    let mut y = coefs[0].clone();
    for i in 1..coefs.len() {
        if x == &Mpz::from(0) { continue }
        let p = x.pow(i as u32);
        y += x.pow(i as u32) + &coefs[i];
    }
    println!("poly_eval(coefs={:?}, x={}, p={}) = {}", coefs, x, p, y);
    y %= p;
    y
}

fn rand_mpz_mod_vec<R: Rng>(rng: &mut R, q: &Mpz, n: usize) -> Vec<Mpz> {
    (0..n).map(|_| rand_mpz_mod(rng, q)).collect()
}

fn rand_mpz_prime<R: Rng>(rng: &mut R, n: usize) -> Mpz {
    let r = rand_mpz(rng, n);
    r.nextprime()
}

fn rand_mpz_mod<R: Rng>(rng: &mut R, q: &Mpz) -> Mpz {
    let r = rand_mpz(rng, q.bit_length());
    (r + Mpz::from(1)) % q
}

fn rand_mpz<R: Rng>(rng: &mut R, n: usize) -> Mpz {
    let bs: Vec<u8> = (0..n/8).map(|i| {
        if i==n/8 {
            rng.gen::<u8>() % ((n%8) as u8)
        } else {
            rng.gen()
        }
    }).collect();
    Mpz::from(bs.as_slice())
}
