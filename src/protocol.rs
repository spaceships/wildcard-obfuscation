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

macro_rules! point {
    ( $i:expr, $j:expr ) => {
        (2*($i+1)+$j) as u64
    };
}

macro_rules! max_point {
    ( $n:expr ) => {
        (2*($n+2)) as u64
    };
}

pub fn get_primes(secparam: usize) -> (Mpz, Mpz) {
    match secparam {
        2048 => (
                Mpz::from_str_radix("6463401214262201460142975337733990392088820533943096\
                    806426069085504931027773578178639440282304582692737743592184379603898\
                    823911830098184219017630477289656624126175473460199218350039550077930\
                    421359211527676813513655358443728523951232367618867695234094116329170\
                    407261008577515178308213161721510479824786077104382866677933668484136\
                    994957312913898971235207065264411615561131866205238541692062830051718\
                    572835423345188720743692371471519670230460329180880739522646657446245\
                    425136942164041945031420345386264693935708516131339587009199453670599\
                    727643105033277887467108720427086645920929063695720990429638711170722\
                    2119196969103", 10).unwrap(),
                Mpz::from_str_radix("3231700607131100730071487668866995196044410266971548\
                    403213034542752465513886789089319720141152291346368871796092189801949\
                    411955915049092109508815238644828312063087736730099609175019775038965\
                    210679605763838406756827679221864261975616183809433847617047058164585\
                    203630504288757589154106580860755239912393038552191433338966834242068\
                    497478656456949485617603532632205807780565933102619270846031415025859\
                    286417711672594360371846185735759835115230164590440369761323328723122\
                    712568471082020972515710172693132346967854258065669793504599726835299\
                    863821552516638943733554360213543322960464531847860495214819355585361\
                    1059598484551", 10).unwrap()
            ),
    //     128 => (
    //         Mpz::from_str_radix("3595386269724631815458610381578049467235953957884613\
    //             145468601623154653516110019262654169546448150720422402277597427867153\
    //             175795376288332449856948612789482487555357868497309705526044392024921\
    //             882389061659041700115376763013646849257629478262210816544743267010213\
    //             69172596479894491876959432609670712659248449113067", 10).unwrap(),
    //         Mpz::from_str_radix("1797693134862315907729305190789024733617976978942306\
    //             572734300811577326758055009631327084773224075360211201138798713933576\
    //             587897688144166224928474306394741243777678934248654852763022196012460\
    //             941194530829520850057688381506823424628814739131105408272371633505106\
    //             84586298239947245938479716304835356329624224556533", 10).unwrap(),
    //     ),

        _ => {
            eprint!("generating {}-bit prime...", secparam);
            let (p, q) = mpz_strong_prime(secparam);
            eprintln!("p={} q={}", p, q);
            (p, q)
        }
    }
}

impl WildcardObfuscation {
    pub fn encode(pat: &str, secparam: usize) -> Self {
        let n = pat.len();
        let ref mut rng = rand::thread_rng();
        let (p,q) = get_primes(secparam);
        let g = Mpz::from(2);

        // generate the random polynomial F with F(0) = 0
        // let ref mut f = rand_mpz_mod_vec(rng, &Mpz::from(secparam as u64), n);
        let ref mut f = rand_mpz_mod_vec(rng, &q, n);
        f[0] = Mpz::from(0);

        // create the h_ij encodings
        let mut h = Vec::with_capacity(pat.len());
        for (i, elem) in pat.chars().enumerate() {
            h.push([Mpz::from(0), Mpz::from(0)]);
            for &j in [0,1].iter() {
                let j_as_char = std::char::from_digit(j as u32, 10).unwrap();

                if elem == j_as_char || elem == '*' {
                    let y = poly_eval(f, &Mpz::from(point!(i,j)));
                    h[i][j] = g.powm(&y, &p);
                } else {
                    h[i][j] = rand_mpz_mod(rng, &p);
                }
            }
        }
        WildcardObfuscation { p, q, h }
    }

    pub fn encode_many(pats_input: &[&str], secparam: usize) -> Self {
        let n = pats_input[0].len();
        for subpat in pats_input {
            if subpat.len() != n {
                panic!("subpattern \"{}\" does not have length {}!", subpat, n);
            }
            if subpat.contains("*") {
                panic!("multi-match does not support wildcards")
            }
        }

        let pats: Vec<Vec<char>> = pats_input.iter().map(|&pat| pat.chars().collect()).collect();
        println!("{:?}", pats);

        let ref mut rng = rand::thread_rng();
        let (p, q) = get_primes(secparam);
        println!("p={} q={}", p, q);
        let g = Mpz::from(2);

        let mut polys: Vec<Vec<(u64, Mpz)>> = Vec::new();

        for pi in 0..pats.len() {
            let mut points = Vec::with_capacity(n);
            points.push((0,Mpz::from(0))); // all polys have (0,0)
            'char_loop: for i in 0..n {
                for pj in 0..pi { // previous patterns
                    for j in 0..2 {
                        let j_as_char = std::char::from_digit(j as u32, 10).unwrap();
                        if pats[pi][i] == j_as_char && pats[pj][i] == j_as_char {
                            points.push((point!(i,j), lagrange_poly_eval(point!(i,j), &polys[pj], &q)));
                            continue 'char_loop;
                        }
                    }
                }
            }
            // we now have points filled all the points from previous polys
            if points.len() >= n {
                println!("pat={:?} failed on {:?} points={:?}", pats_input, pats_input[pi], points);
                assert!(points.len() < n, "ran out of freedom!");
            }
            let mut ctr: u64 = 0;
            while points.len() < n {
                // just pick unused points
                let y = rand_mpz_mod(rng, &Mpz::from(secparam as u64));
                println!("y={}", y);
                points.push((max_point!(n)+ctr, y));
                ctr += 1;
            }
            polys.push(points);
        }

        println!("{:?}", polys);

        // create the h_ij encodings
        let mut h = Vec::with_capacity(n);
        for i in 0..n {
            h.push([Mpz::from(0), Mpz::from(0)]);
            for j in 0..2 {
                let mut found_match = false;
                for pi in 0..pats.len() {
                    let j_as_char = std::char::from_digit(j as u32, 10).unwrap();
                    if pats[pi][i] == j_as_char {
                        let y = lagrange_poly_eval(point!(i,j), &polys[pi], &q);
                        h[i][j] = g.powm(&y, &p);
                        found_match = true;
                        break;
                    }
                }
                if !found_match {
                    h[i][j] = rand_mpz_mod(rng, &p);
                }
            }
        }

        println!("{:?}", h);

        WildcardObfuscation { p, q, h }
    }

    pub fn eval(&self, inp: &str) -> usize {
        assert_eq!(inp.len(), self.h.len(),
            "error: expected {}-bit input, but got {} bits!", self.h.len(), inp.len());

        let x: Vec<usize> = inp.chars().map(|c| {
            c.to_digit(2).expect("expected a binary digit!") as usize
        }).collect();

        let mut t = Mpz::from(1);
        let pts: Vec<u64> = x.iter().enumerate().map(|(i, &x)| point!(i, x)).collect();
        for i in 0..inp.len() {
            // compute lagrange coeficient in the exponent group
            let exp = lagrange_coef(i as u64, 0, pts.as_slice(), &self.q);
            let val = self.h[i][x[i]].powm(&exp, &self.p);
            t *= val;
            t %= &self.p;
        }

        (t == Mpz::from(1)) as usize
    }
}

fn poly_eval(coefs: &Vec<Mpz>, x: &Mpz) -> Mpz {
    let mut y = coefs[0].clone();
    for i in 1..coefs.len() {
        if x == &Mpz::from(0) { continue }
        y += &coefs[i] * x.pow(i as u32);
    }
    y
}

fn lagrange_poly_eval(interp_at: u64, points: &[(u64, Mpz)], q: &Mpz) -> Mpz {
    let mut acc = Mpz::from(0);
    let xs: Vec<u64> = points.iter().map(|pt| pt.0).collect();
    for i in 0..points.len() {
        let lag = lagrange_coef(i as u64, interp_at, xs.as_slice(), q);
        acc += &points[i].1 * lag;
        acc = acc.modulus(q);
    }
    acc
}

fn lagrange_coef(i: u64, x: u64, xs: &[u64], q: &Mpz) -> Mpz {
    let mut prod = Mpz::from(1);
    let ref px = Mpz::from(x);
    for j in 0..xs.len() {
        if i == j as u64 { continue }
        let ref pi = Mpz::from(xs[i as usize]);
        let ref pj = Mpz::from(xs[j as usize]);
        prod *= (px - pj) * (pi - pj).invert(q).expect("couldn't invert!");
        prod = prod.modulus(q);
    }
    prod
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

#[cfg(test)]
mod tests {
    use super::*;
    use rand::Rng;

    #[test]
    fn simple_pattern() {
        for _ in 0..1 {
            let obf = WildcardObfuscation::encode("0*10", 2048);
            assert_eq!(obf.eval("0110"), 1);
            assert_eq!(obf.eval("0010"), 1);
            assert_eq!(obf.eval("1010"), 0);
            assert_eq!(obf.eval("0011"), 0);
        }
    }

    #[test]
    fn multi_pattern() {
        for _ in 0..16 {
            let obf = WildcardObfuscation::encode_many(&["011000", "101100", "000001"], 128);
            assert_eq!(obf.eval("011000"), 1);
            assert_eq!(obf.eval("101100"), 1);
            assert_eq!(obf.eval("101000"), 0);
            assert_eq!(obf.eval("011100"), 0);
        }
    }

    #[test]
    fn mpz_modulus() {
        let x = Mpz::from(-4).modulus(&Mpz::from(5));
        assert_eq!(x, Mpz::from(1));
    }

    #[test]
    fn test_lagrange_poly_eval() {
        let n = 16;
        let ref mut rng = rand::thread_rng();
        let (p,q) = get_primes(128);
        let ref f = rand_mpz_mod_vec(rng, &q, n-1);
        for _ in 0..16 {
            // get n random points
            let mut points = Vec::new();
            for _ in 0..n {
                let x : u64 = rng.gen();
                let y = poly_eval(f, &Mpz::from(x));
                points.push((x,y));
            }
            for _ in 0..16 {
                let x : u64 = rng.gen();
                let y = poly_eval(f, &Mpz::from(x)) % &p;
                assert_eq!(lagrange_poly_eval(x, &points, &p), y);
            }
        }
    }

    #[test]
    fn multi_interpolation() {
        let n = 10;
        let ref mut rng = rand::thread_rng();
        let (_, ref q) = get_primes(2048);

        let mut f1 = Vec::with_capacity(n);
        f1.push((0, Mpz::from(0)));
        let mut ctr = 0;
        while f1.len() < n {
            let y = rand_mpz_mod(rng, q);
            f1.push((max_point!(n) + ctr, y));
            ctr += 1;
        }
        println!("{:?}", f1);

        let mut f2 = Vec::with_capacity(n);
        for i in 0..n {
            let x = point!(i,0);
            f2.push((x, lagrange_poly_eval(x, &f1, q)));
        }

        assert_eq!(lagrange_poly_eval(0, &f2, q), Mpz::from(0));
        for _ in 0..128 {
            let x = rng.gen();
            assert_eq!(lagrange_poly_eval(x, &f1, q), lagrange_poly_eval(x, &f2, q));
        }
    }
}
