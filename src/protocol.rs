use rand_mpz::*;

use gmp::mpz::Mpz;
use rand;
use std;
use std::io::{Read, Write, BufWriter};
use std::fs::File;

pub struct WildcardObfuscation {
    p: Mpz,             // prime modulus
    q: Mpz,             // prime modulus for the exponent
    h: Vec<[Mpz; 2]>,   // h_ij encodings
}

pub fn point(i: usize, j: usize) -> i32 {
    2*(i as i32 + 1) + j as i32
}

impl WildcardObfuscation {

    pub fn encode(pat: &str, secparam: usize) -> Self {
        let n = pat.len();

        let ref mut rng = rand::thread_rng();

        // we precompute some primes for benchmarking
        let (p, q) = match secparam {
            1024 => (
                Mpz::from_str_radix("3595386269724631815458610381578049467235953957884613\
                    145468601623154653516110019262654169546448150720422402277597427867153\
                    175795376288332449856948612789482487555357868497309705526044392024921\
                    882389061659041700115376763013646849257629478262210816544743267010213\
                    69172596479894491876959432609670712659248449113067", 10).unwrap(),
                Mpz::from_str_radix("1797693134862315907729305190789024733617976978942306\
                    572734300811577326758055009631327084773224075360211201138798713933576\
                    587897688144166224928474306394741243777678934248654852763022196012460\
                    941194530829520850057688381506823424628814739131105408272371633505106\
                    84586298239947245938479716304835356329624224556533", 10).unwrap(),
            ),

            _ => {
                eprint!("generating {}-bit prime...", secparam);
                let (p, q) = mpz_strong_prime(secparam);
                eprintln!("p={} q={}", p, q);
                (p, q)
            }
        };

        let g = Mpz::from(2);

        // generate the random polynomial F with F(0) = 0
        let ref f = rand_mpz_mod_vec(rng, &p, n-1);

        // create the h_ij encodings
        let mut h = Vec::with_capacity(pat.len());
        for (i, elem) in pat.chars().enumerate() {
            h.push([Mpz::from(0), Mpz::from(0)]);

            for &j in [0,1].iter() {
                let j_as_char = std::char::from_digit(j as u32, 10).unwrap();

                if elem == j_as_char || elem == '*' {
                    let y = poly_eval(f, &Mpz::from(point(i,j)));
                    h[i][j] = g.powm(&y, &p);
                } else {
                    h[i][j] = rand_mpz_mod(rng, &p);
                }
            }
        }

        WildcardObfuscation { p, q, h }
    }

    pub fn eval(&self, inp: &str) -> usize {
        assert_eq!(inp.len(), self.h.len(),
            "error: expected {}-bit input, but got {} bits!", self.h.len(), inp.len());

        let x: Vec<usize> = inp.chars().map(|c| {
            c.to_digit(2).expect("expected a binary digit!") as usize
        }).collect();

        let mut t = Mpz::from(1);
        for i in 0..inp.len() {
            // compute lagrange coeficient in the exponent group
            let exp = lagrange_coef(i, &x, &self.q);
            let val = self.h[i][x[i]].powm(&exp, &self.p);
            t *= val;
            t %= &self.p;
        }

        (t == Mpz::from(1)) as usize
    }
}

fn lagrange_coef(i: usize, x: &[usize], q: &Mpz) -> Mpz {
    let mut prod = Mpz::from(1);
    for j in 0..x.len() {
        if i == j { continue }
        let pi = Mpz::from(point(i, x[i]));
        let pj = Mpz::from(point(j, x[j]));
        prod *= (-&pj) * (&pi - &pj).invert(q).expect("couldn't invert!") % q;
        prod %= q;
    }
    prod
}

fn poly_eval(coefs: &Vec<Mpz>, x: &Mpz) -> Mpz {
    let mut y = Mpz::from(0);
    for i in 0..coefs.len() {
        if x == &Mpz::from(0) { continue }
        y += &coefs[i] * x.pow(i as u32 + 1)
    }
    y
}

////////////////////////////////////////////////////////////////////////////////
// serialization

impl WildcardObfuscation {
    pub fn to_file(&self, filename: &str) {
        let file = File::create(filename).expect("could not create file!");
        let mut buf = BufWriter::new(file);
        self.write(&mut buf);
    }

    pub fn write<W: Write>(&self, f: &mut W) {
        writeln!(f, "{}\n{}\n{}", self.h.len(), self.p, self.q).unwrap();
        for i in 0..self.h.len() {
            for j in 0..2 {
                writeln!(f, "{}", self.h[i][j]).unwrap();
            }
        }
    }

    pub fn from_file(filename: &str) -> Self {
        let mut file = File::open(filename).expect("Unable to open the file");
        let mut contents = String::new();
        file.read_to_string(&mut contents).expect("Unable to read the file");
        Self::read(&contents)
    }

    pub fn read(contents: &str) -> Self {
        let mut lines = contents.lines();
        let n: usize = lines.next().expect("expected a line!")
            .parse().expect("expected line 1 to be a number in base 10!");
        let p: Mpz = Mpz::from_str_radix(lines.next().expect("expected a line!"), 10)
            .expect("expected line 2 to be p in base 10!");
        let q: Mpz = Mpz::from_str_radix(lines.next().expect("expected a line!"), 10)
            .expect("expected line 3 to be q in base 10!");
        let mut h = vec![[Mpz::from(0), Mpz::from(0)]; n];
        for i in 0..n {
            for j in 0..2 {
                h[i][j] = Mpz::from_str_radix(lines.next().expect("expected a line!"), 10)
                    .expect("expected line to be base 10!");
            }
        }
        assert_eq!(lines.next(), Option::None);
        WildcardObfuscation { p, q, h }
    }
}
