use rand_mpz::*;
use alt_pattern::*;

use gmp::mpz::Mpz;
use rand;
use std;
use std::io::{Read, Write, BufWriter};
use std::fs::File;

pub struct Obf {
    p: Mpz,             // prime modulus
    q: Mpz,             // prime modulus for the exponent
    h: Vec<Vec<Mpz>>,   // h_ij encodings
    n: usize,
    l: usize,
}

macro_rules! point {
    ( $i:expr, $j:expr ) => {
        (2*$i+$j+1) as u64
    };
}

macro_rules! multi_point {
    ( $n:expr, $i:expr, $j:expr, $xi:expr, $xj:expr ) => {
        (4*$n*$i + 4*$j + 2*$xi + $xj + 1) as u64
    };
}

const BIT_CHARS: [(usize, char); 2] = [(0,'0'), (1,'1')];

pub fn get_primes(secparam: usize) -> (Mpz, Mpz) {
    match secparam {
        1024 => (
                Mpz::from_str_radix("35953862697246318154586103815780494672359539578846131\
                    4546860162315465351611001926265416954644815072042240227759742786715317\
                    5795376288332449856948612789482487555357868497309705526044392024921882\
                    3890616590417001153767630136468492576294782622108165447432670102136917\
                    2596479894491876959432609670712659248449113067", 10).unwrap(),
                Mpz::from_str_radix("17976931348623159077293051907890247336179769789423065\
                    7273430081157732675805500963132708477322407536021120113879871393357658\
                    7897688144166224928474306394741243777678934248654852763022196012460941\
                    1945308295208500576883815068234246288147391311054082723716335051068458\
                    6298239947245938479716304835356329624224556533", 10).unwrap(),
        ),
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
            h.push(vec![Mpz::from(0), Mpz::from(0)]);
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
        Obf { p, q, h, n, l:0 }
    }

    pub fn multimatch(pat_input: &str, secparam: usize) -> Self {

        let pat = AltPattern::new(pat_input);
        let n = pat.len();

        let l = pat.max_alt_len();

        // eprintln!("pat={} n={}", pat_input, n);

        let ref mut rng = rand::thread_rng();
        let (p, q) = get_primes(secparam);
        let g = Mpz::from(2);

        let ref mut f = rand_mpz_mod_vec(rng, &q, n*l);
        f[0] = Mpz::from(0);

        let mut h: Vec<Vec<Mpz>> = vec![vec![Mpz::from(0), Mpz::from(0), Mpz::from(0), Mpz::from(0)]; n*l];

        for i in 0..n {
            for j in 0..l {
                // eprintln!("");
                for &(x,xchar) in BIT_CHARS.iter() {
                    for &(y,ychar) in BIT_CHARS.iter() {
                        if pat.matches_both(i, xchar, j, ychar) {
                            // eprintln!("h[{}][{}] = F", n*i+j, 2*x+y);
                            h[l*i+j][2*x+y] = g.powm(&poly_eval(f, &Mpz::from(multi_point!(n,i,j,x,y))), &p);
                        } else {
                            // eprintln!("h[{}][{}] = $", n*i+j, 2*x+y);
                            h[l*i+j][2*x+y] = rand_mpz_mod(rng, &p);
                        }
                    }
                }
            }
        }

        Obf { p, q, h, n, l }
    }

    pub fn eval(&self, inp: &str) -> usize {
        let n = self.n;
        let l = self.l;
        assert_eq!(inp.len(), n,
            "error: expected {}-bit input, but got {} bits!", n, inp.len());

        let x: Vec<usize> = inp.chars().map(|c| {
            c.to_digit(2).expect("expected a binary digit!") as usize
        }).collect();

        let mut t = Mpz::from(1);

        if l > 0 {
            let mut pts   = Vec::with_capacity(n*l);
            let mut order = Vec::with_capacity(n*l);
            for i in 0..n {
                for j in 0..l {
                    order.push((i, j, pts.len() as u64));
                    pts.push(multi_point!(n, i, j, x[i], x[j]));
                }
            }

            for (i, j, ix) in order.into_iter() {
                let exp = lagrange_coef(ix, 0, pts.as_slice(), &self.q);
                let val = self.h[l*i+j][2*x[i]+x[j]].powm(&exp, &self.p);
                t *= val;
                t %= &self.p;
            }

            (t == Mpz::from(1)) as usize

        } else {
            let pts: Vec<u64> = x.iter().enumerate().map(|(i, &x)| point!(i, x)).collect();
            for i in 0..n {
                // compute lagrange coeficient in the exponent group
                let exp = lagrange_coef(i as u64, 0, pts.as_slice(), &self.q);
                let val = self.h[i][x[i]].powm(&exp, &self.p);
                t *= val;
                t %= &self.p;
            }

            (t == Mpz::from(1)) as usize
        }

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

#[allow(dead_code)]
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
        writeln!(f, "{}", self.l);
        writeln!(f, "{}\n{}\n{}", self.n, self.p, self.q).unwrap();
        if self.l > 0 {
            for i in 0..self.n {
                for j in 0..self.l {
                    for xi in 0..2 {
                        for xj in 0..2 {
                            writeln!(f, "{}", self.h[self.l*i + j][2*xi + xj]).unwrap();
                        }
                    }
                }
            }
        } else {
            for i in 0..self.h.len() {
                for j in 0..2 {
                    writeln!(f, "{}", self.h[i][j]).unwrap();
                }
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
        let l: usize = lines.next().expect("expected a line!")
            .parse().expect("expected line 1 to be a number in base 10!");
        let n: usize = lines.next().expect("expected a line!")
            .parse().expect("expected line 1 to be a number in base 10!");
        let p: Mpz = Mpz::from_str_radix(lines.next().expect("expected a line!"), 10)
            .expect("expected line 2 to be p in base 10!");
        let q: Mpz = Mpz::from_str_radix(lines.next().expect("expected a line!"), 10)
            .expect("expected line 3 to be q in base 10!");
        let mut h;
        if l > 0 {
            h = vec![vec![Mpz::from(0), Mpz::from(0), Mpz::from(0), Mpz::from(0)]; n*l];
            for i in 0..n {
                for j in 0..l {
                    for xi in 0..2 {
                        for xj in 0..2 {
                            h[l*i+j][2*xi+xj] = Mpz::from_str_radix(lines.next().expect("expected a line!"), 10)
                                .expect("expected line to be base 10!");
                        }
                    }
                }
            }
        } else {
            h = vec![vec![Mpz::from(0), Mpz::from(0)]; n];
            for i in 0..n {
                for j in 0..2 {
                    h[i][j] = Mpz::from_str_radix(lines.next().expect("expected a line!"), 10)
                        .expect("expected line to be base 10!");
                }
            }
        }
        assert_eq!(lines.next(), None);
        Obf { p, q, h, n, l }
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
        let obf = Obf::multimatch("1", 2048);
        assert_eq!(obf.eval("1"), 1);
        assert_eq!(obf.eval("0"), 0);

        let obf = Obf::multimatch("01", 2048);
        assert_eq!(obf.eval("01"), 1);
        assert_eq!(obf.eval("10"), 0);
        assert_eq!(obf.eval("00"), 0);
        assert_eq!(obf.eval("11"), 0);

        let obf = Obf::multimatch("(00|11)", 2048);
        assert_eq!(obf.eval("00"), 1);
        assert_eq!(obf.eval("11"), 1);
        assert_eq!(obf.eval("01"), 0);
        assert_eq!(obf.eval("10"), 0);

        let obf = Obf::multimatch("(00|11|01)", 2048);
        assert_eq!(obf.eval("00"), 1);
        assert_eq!(obf.eval("11"), 1);
        assert_eq!(obf.eval("01"), 1);
        assert_eq!(obf.eval("10"), 0);

        let obf = Obf::multimatch("(011000|101100|000000)", 2048);
        assert_eq!(obf.eval("011000"), 1);
        assert_eq!(obf.eval("101100"), 1);
        assert_eq!(obf.eval("101000"), 0);
        assert_eq!(obf.eval("011100"), 0);

        let obf = Obf::multimatch("(0**|111)", 2048);
        assert_eq!(obf.eval("000"), 1);
        assert_eq!(obf.eval("001"), 1);
        assert_eq!(obf.eval("100"), 0);
        assert_eq!(obf.eval("101"), 0);
        assert_eq!(obf.eval("111"), 1);

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
