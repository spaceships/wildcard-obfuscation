use alt_pattern::*;

use rand::{self, Rng, CryptoRng};
use std;
use std::io::{Read, Write, BufWriter};
use std::fs::File;

use curve25519_dalek::ristretto::{RistrettoPoint, CompressedRistretto, RistrettoBasepointTable};
use curve25519_dalek::scalar::Scalar;
use curve25519_dalek::traits::{Identity, VartimeMultiscalarMul};
use curve25519_dalek::constants::RISTRETTO_BASEPOINT_TABLE;

type Enc = RistrettoPoint;
const G: RistrettoBasepointTable = RISTRETTO_BASEPOINT_TABLE;

fn id() -> RistrettoPoint {
    RistrettoPoint::identity()
}

pub struct Obf {
    h: Vec<Vec<Enc>>,   // h_ij encodings
    n: usize,
    l: usize,
}

macro_rules! point {
    ( $i:expr, $j:expr ) => {
        2*$i+$j+1
    };
}

macro_rules! multi_point {
    ( $n:expr, $i:expr, $j:expr, $xi:expr, $xj:expr ) => {
        4*$n*$i + 4*$j + 2*$xi + $xj + 1
    };
}

const BIT_CHARS: [(usize, char); 2] = [(0,'0'), (1,'1')];

fn rand_scalar_vec<R: Rng + CryptoRng>(rng: &mut R, n: usize) -> Vec<Scalar> {
    (0..n).map(|_| Scalar::random(rng)).collect()
}

impl Obf {
    pub fn encode(pat: &str) -> Self {
        let n = pat.len();
        let ref mut rng = rand::thread_rng();

        // generate the random polynomial F with F(0) = 0
        let ref mut f = rand_scalar_vec(rng, n);
        f[0] = Scalar::zero();

        // create the h_ij encodings
        let mut h = Vec::with_capacity(pat.len());
        for (i, elem) in pat.chars().enumerate() {
            h.push(vec![id(), id()]);
            for &j in [0,1].iter() {
                let j_as_char = std::char::from_digit(j as u32, 10).unwrap();

                if elem == j_as_char || elem == '*' {
                    h[i][j] = &G * &poly_eval(f, &Scalar::from(point!(i,j) as u64));
                } else {
                    h[i][j] = Enc::random(rng);
                }
            }
        }
        Obf { h, n, l:0 }
    }

    pub fn multimatch(pat_input: &str) -> Self {

        let pat = AltPattern::new(pat_input);
        let n = pat.len();

        let l = pat.max_alt_len();

        let ref mut rng = rand::thread_rng();

        let ref mut f = rand_scalar_vec(rng, n*l);
        f[0] = Scalar::zero();

        let mut h = vec![vec![id(), id(), id(), id()]; n*l];

        for i in 0..n {
            for j in 0..l {
                for &(x,xchar) in BIT_CHARS.iter() {
                    for &(y,ychar) in BIT_CHARS.iter() {
                        if pat.matches_both(i, xchar, j, ychar) {
                            h[l*i+j][2*x+y] = &G * &poly_eval(f, &Scalar::from(multi_point!(n,i,j,x,y) as u64));
                        } else {
                            h[l*i+j][2*x+y] = Enc::random(rng);
                        }
                    }
                }
            }
        }

        Obf { h, n, l }
    }

    pub fn eval(&self, inp: &str) -> usize {
        let n = self.n;
        let l = self.l;
        assert_eq!(inp.len(), n,
            "error: expected {}-bit input, but got {} bits!", n, inp.len());

        let x: Vec<usize> = inp.chars().map(|c| {
            c.to_digit(2).expect("expected a binary digit!") as usize
        }).collect();

        if l > 0 {
            let mut pts   = Vec::with_capacity(n*l);
            let mut order = Vec::with_capacity(n*l);
            for i in 0..n {
                for j in 0..l {
                    order.push((i, j, pts.len()));
                    pts.push(multi_point!(n, i, j, x[i], x[j]));
                }
            }

            let mut scalars = Vec::new();
            let mut encs = Vec::new();
            for (i, j, ix) in order.into_iter() {
                scalars.push(lagrange_coef(ix, 0, pts.as_slice()));
                encs.push(self.h[l*i+j][2*x[i]+x[j]]);
            }
            let res = RistrettoPoint::vartime_multiscalar_mul(scalars.iter(), encs.iter());

            (res == RistrettoPoint::identity()) as usize

        } else {
            let pts: Vec<usize> = x.iter().enumerate().map(|(i, &x)| point!(i, x)).collect();
            let scalars = (0..n).map(|i| lagrange_coef(i, 0, pts.as_slice()));
            let encs = (0..n).map(|i| self.h[i][x[i]]);
            let res = RistrettoPoint::vartime_multiscalar_mul(scalars, encs);

            (res == RistrettoPoint::identity()) as usize
        }

    }
}

fn scalar_pow(x: &Scalar, n: usize) -> Scalar {
    let mut y = Scalar::one();
    for _ in 0..n {
        y *= x;
    }
    y
}

fn poly_eval(coefs: &Vec<Scalar>, x: &Scalar) -> Scalar {
    let mut y = coefs[0].clone();
    if x == &Scalar::zero() {
        return y;
    }
    for i in 1..coefs.len() {
        y += coefs[i] * scalar_pow(x, i);
    }
    y
}

fn lagrange_coef(i: usize, x: usize, xs: &[usize]) -> Scalar {
    let mut prod = Scalar::one();
    let ref px = Scalar::from(x as u64);
    for j in 0..xs.len() {
        if i == j { continue }
        let ref pi = Scalar::from(xs[i] as u64);
        let ref pj = Scalar::from(xs[j] as u64);
        prod *= (px - pj) * (pi - pj).invert();
        prod.reduce();
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
        writeln!(f, "{}", self.l).unwrap();
        writeln!(f, "{}", self.n).unwrap();
        if self.l > 0 {
            for i in 0..self.n {
                for j in 0..self.l {
                    for xi in 0..2 {
                        for xj in 0..2 {
                            f.write(self.h[self.l*i + j][2*xi + xj].compress().as_bytes()).unwrap();
                            writeln!(f, "").unwrap();
                        }
                    }
                }
            }
        } else {
            for i in 0..self.h.len() {
                for j in 0..2 {
                    f.write(self.h[i][j].compress().as_bytes()).unwrap();
                    writeln!(f, "").unwrap();
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
        let mut h;
        if l > 0 {
            h = vec![vec![id(), id(), id(), id()]; n*l];
            for i in 0..n {
                for j in 0..l {
                    for xi in 0..2 {
                        for xj in 0..2 {
                            let s = lines.next().expect("expected a line!");
                            let mut arr: [u8; 32] =  [0; 32];
                            arr.copy_from_slice(s.as_bytes());
                            h[l*i+j][2*xi+xj] = CompressedRistretto(arr).decompress()
                                .expect("error decompressing Ristretto");
                        }
                    }
                }
            }
        } else {
            h = vec![vec![id(), id()]; n];
            for i in 0..n {
                for j in 0..2 {
                    let s = lines.next().expect("expected a line!");
                    let mut arr: [u8; 32] =  [0; 32];
                    arr.copy_from_slice(s.as_bytes());
                    h[i][j] = CompressedRistretto(arr).decompress()
                        .expect("error decompressing Ristretto");
                }
            }
        }
        assert_eq!(lines.next(), None);
        Obf { h, n, l }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_pattern() {
        // for _ in 0..1 {
            let obf = Obf::encode("0*10");
            assert_eq!(obf.eval("0110"), 1);
            assert_eq!(obf.eval("0010"), 1);
            assert_eq!(obf.eval("1010"), 0);
            assert_eq!(obf.eval("0011"), 0);
        // }
    }

    #[test]
    fn test_multimatch() {
        let obf = Obf::multimatch("01");
        assert_eq!(obf.eval("01"), 1);
        assert_eq!(obf.eval("10"), 0);
        assert_eq!(obf.eval("00"), 0);
        assert_eq!(obf.eval("11"), 0);

        let obf = Obf::multimatch("(00|11)");
        assert_eq!(obf.eval("00"), 1);
        assert_eq!(obf.eval("11"), 1);
        assert_eq!(obf.eval("01"), 0);
        assert_eq!(obf.eval("10"), 0);

        let obf = Obf::multimatch("(00|11|01)");
        assert_eq!(obf.eval("00"), 1);
        assert_eq!(obf.eval("11"), 1);
        assert_eq!(obf.eval("01"), 1);
        assert_eq!(obf.eval("10"), 0);

        let obf = Obf::multimatch("(011000|101100|000000)");
        assert_eq!(obf.eval("011000"), 1);
        assert_eq!(obf.eval("101100"), 1);
        assert_eq!(obf.eval("101000"), 0);
        assert_eq!(obf.eval("011100"), 0);

        let obf = Obf::multimatch("(0**|111)");
        assert_eq!(obf.eval("000"), 1);
        assert_eq!(obf.eval("001"), 1);
        assert_eq!(obf.eval("100"), 0);
        assert_eq!(obf.eval("101"), 0);
        assert_eq!(obf.eval("111"), 1);

    }

    #[test]
    fn test_scalar_pow() {
        assert_eq!(scalar_pow(&Scalar::from(10 as u64), 10), Scalar::from((10 as u64).pow(10)));
        assert_eq!(scalar_pow(&Scalar::from(10 as u64), 0), Scalar::from((10 as u64).pow(0)));
    }

    #[test]
    fn test_encodings() {
        let ref mut rng = rand::thread_rng();

        for _ in 0..16 {
            let x = Scalar::random(rng);
            let y = Scalar::random(rng);
            assert_eq!(&G*&x + &G*&y, &G*&(x+y));
        }
    }

    // test Lagrange with Scalars
    #[test]
    fn test_lagrange() {
        let ref mut rng = rand::thread_rng();
        let n = 16;
        for _ in 0..16 {
            let ref mut f = rand_scalar_vec(rng, n);
            let mut xs = Vec::new();
            let mut ys = Vec::new();
            for x in 1..n+1 {
                let y = poly_eval(&f, &Scalar::from(x as u64));
                xs.push(x);
                ys.push(y);
            }
            let mut acc = Scalar::zero();
            for i in 0..n {
                acc += lagrange_coef(i, 0, xs.as_slice()) * ys[i];
            }
            assert_eq!(acc, f[0]);
        }
    }

    // test Lagrange in the exponent
    #[test]
    fn test_lagrange_exp() {
        let ref mut rng = rand::thread_rng();
        let n = 16;
        for _ in 0..4 {
            let ref mut f = rand_scalar_vec(rng, n);
            f[0] = Scalar::zero();
            let mut xs = Vec::new();
            let mut ys = Vec::new();
            for x in 1..n+1 {
                let y = poly_eval(&f, &Scalar::from(x as u64));
                xs.push(x);
                ys.push(y);
            }
            let mut acc = (&G * &ys[0]) * lagrange_coef(0, 0, xs.as_slice());
            for i in 1..n {
                acc += (&G * &ys[i]) * lagrange_coef(i, 0, xs.as_slice());
            }
            assert_eq!(acc, RistrettoPoint::identity());
        }
    }
}
