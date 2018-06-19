use rand_mpz::*;

use gmp::mpz::Mpz;
use rand;
use std;
use std::io::{Read, Write, BufWriter};
use std::fs::File;

pub struct Obf {
    p: Mpz,             // prime modulus
    q: Mpz,             // prime modulus for the exponent
    h: Vec<[Mpz; 2]>,   // h_ij encodings
    multi: bool,
}

macro_rules! point {
    ( $i:expr, $j:expr ) => {
        (2*$i+$j+1) as u64
    };
}

macro_rules! multi_point {
    ( $i:expr, $j:expr, $k:expr ) => {
        if $i==0 {
            (2*$j+$k+1) as u64
        } else {
            (2*$i+$j+5) as u64
        }
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
        _ => {
            eprint!("generating {}-bit prime...", secparam);
            let (p, q) = mpz_strong_prime(secparam);
            eprintln!("p={} q={}", p, q);
            (p, q)
        }
    }
}

impl Obf {
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
        Obf { p, q, h, multi: false }
    }

    pub fn multimatch(pats_input: &[&str], secparam: usize) -> Self {
        let n = pats_input[0].len();
        for subpat in pats_input {
            if subpat.len() != n {
                panic!("subpattern \"{}\" does not have length {}!", subpat, n);
            }
        }

        let pats: Vec<Vec<char>> = pats_input.iter().map(|&pat| pat.chars().collect()).collect();

        let ref mut rng = rand::thread_rng();
        let (p, q) = get_primes(secparam);
        let g = Mpz::from(2);

        // we need (2*n)+1 h_ij's so there is enough flexibility for overlapping patterns
        let mut h: Vec<[Mpz; 2]> = vec![[Mpz::from(0), Mpz::from(0)]; n+1];
        let mut h_defined: Vec<[bool; 2]> = vec![[false; 2]; n];

        for pi in 0..pats.len() {
            let mut constraints = Vec::with_capacity(n);
            constraints.push((0, Mpz::from(0)));

            // find overlapping points
            for i in 0..n {
                for j in 0..2 {
                    let j_as_char = std::char::from_digit(j as u32, 10).unwrap();
                    if pats[pi][i] == j_as_char && h_defined[i][j] {
                        if i == 0 {
                            constraints.push((multi_point!(0,j,0), h[0][j].clone()));
                            constraints.push((multi_point!(0,j,1), h[1][j].clone()));
                        } else {
                            constraints.push((multi_point!(i,j,0), h[i+1][j].clone()));
                        }
                    }
                }
            }

            println!("pi={} nconstraints={}", pi, constraints.len());

            assert!(constraints.len() <= n+1, "this pattern is completely determined by previous ones!");

            // interpolate/create new points
            for i in 0..n {
                for j in 0..2 {
                    let j_as_char = std::char::from_digit(j as u32, 10).unwrap();
                    if pats[pi][i] == j_as_char && !h_defined[i][j] {
                        if constraints.len() < n+1 {
                            h[i+1][j] = rand_mpz_mod(rng, &Mpz::from(secparam as u64));
                            constraints.push((multi_point!(i,j,1), h[i+1][j].clone()));
                            if i == 0 {
                                if constraints.len() < n+1 {
                                    h[i][j] = rand_mpz_mod(rng, &Mpz::from(secparam as u64));
                                    constraints.push((multi_point!(i,j,0), h[i][j].clone()));
                                    println!("h_{{{},{}}} <- $", i,j);
                                } else {
                                    h[i][j] = lagrange_poly_eval(multi_point!(i,j,0), constraints.as_slice(), &q);
                                    println!("h_{{{},{}}} <- F", i,j);
                                }
                            }
                            println!("h_{{{},{}}} <- $", i+1,j);
                        } else {
                            h[i+1][j] = lagrange_poly_eval(multi_point!(i,j,1), constraints.as_slice(), &q);
                            if i == 0 {
                                h[i][j] = lagrange_poly_eval(multi_point!(i,j,0), constraints.as_slice(), &q);
                                println!("h_{{{},{}}} <- F", i,j);
                            }
                            println!("h_{{{},{}}} <- F", i+1,j);
                        }
                        h_defined[i][j] = true;
                    }
                }
            }
        }

        // create the h_ij encodings
        for i in 0..n {
            for j in 0..2 {
                if h_defined[i][j] {
                    h[i+1][j] = g.powm(&h[i+1][j], &p);
                    if i == 0 {
                        h[i][j] = g.powm(&h[i][j], &p);
                    }
                } else {
                    h[i+1][j] = rand_mpz_mod(rng, &p);
                    if i == 0 {
                        h[i][j] = rand_mpz_mod(rng, &p);
                    }
                }
            }
        }

        Obf { p, q, h, multi: true }
    }

    pub fn eval(&self, inp: &str) -> usize {
        let n = if self.multi { self.h.len() - 1 } else { self.h.len() };
        assert_eq!(inp.len(), n,
            "error: expected {}-bit input, but got {} bits!", n, inp.len());

        let x: Vec<usize> = inp.chars().map(|c| {
            c.to_digit(2).expect("expected a binary digit!") as usize
        }).collect();

        let mut t = Mpz::from(1);
        let mut pts;
        if self.multi {
            // contortions to deal with extra encodings in the first h_ij slot
            pts = Vec::with_capacity(n+1);
            pts.push(multi_point!(0, x[0], 0));
            pts.push(multi_point!(0, x[0], 1));
            for i in 1..inp.len() {
                pts.push(multi_point!(i, x[i], 0));
            }

            let exp = lagrange_coef(0, 0, pts.as_slice(), &self.q);
            t *= self.h[0][x[0]].powm(&exp, &self.p);
            t %= &self.p;

            let exp = lagrange_coef(1, 0, pts.as_slice(), &self.q);
            t *= self.h[1][x[0]].powm(&exp, &self.p);
            t %= &self.p;

            for i in 1..n {
                let exp = lagrange_coef(1 + i as u64, 0, pts.as_slice(), &self.q);
                let val = self.h[i+1][x[i]].powm(&exp, &self.p);
                t *= val;
                t %= &self.p;
            }
        } else {
            pts = x.iter().enumerate().map(|(i, &x)| point!(i, x)).collect();
            for i in 0..n {
                // compute lagrange coeficient in the exponent group
                let exp = lagrange_coef(i as u64, 0, pts.as_slice(), &self.q);
                let val = self.h[i][x[i]].powm(&exp, &self.p);
                t *= val;
                t %= &self.p;
            }
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

impl Obf {
    pub fn to_file(&self, filename: &str) {
        let file = File::create(filename).expect("could not create file!");
        let mut buf = BufWriter::new(file);
        self.write(&mut buf);
    }

    pub fn write<W: Write>(&self, f: &mut W) {
        if self.multi {
            writeln!(f, "multi").unwrap();
        } else {
            writeln!(f, "single").unwrap();
        }
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
        let multi: bool = match lines.next().expect("expected a line!") {
            "multi" => true,
            "single" => false,
            other => panic!("unknown value: {}!", other),
        };
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
        assert_eq!(lines.next(), None);
        Obf { p, q, h, multi }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rand::Rng;

    #[test]
    fn simple_pattern() {
        for _ in 0..1 {
            let obf = Obf::encode("0*10", 2048);
            assert_eq!(obf.eval("0110"), 1);
            assert_eq!(obf.eval("0010"), 1);
            assert_eq!(obf.eval("1010"), 0);
            assert_eq!(obf.eval("0011"), 0);
        }
    }

    #[test]
    fn test_multimatch() {
        for _ in 0..16 {
            let obf = Obf::multimatch(&["011000", "101100", "000000"], 128);
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
            f1.push((point!(n,1) + ctr, y));
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
