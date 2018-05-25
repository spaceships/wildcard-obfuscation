#![feature(test)]
extern crate wildcard;
extern crate test;
extern crate rand;

use wildcard::protocol::*;

use test::Bencher;
use rand::{thread_rng, Rng};
use std::char::from_digit;

fn rand_inp<R: Rng>(rng: &mut R, n: usize) -> String {
    (0..n).map(|_| from_digit(rng.gen_range(0,2), 10).unwrap() ).collect()
}

fn rand_pat<R: Rng>(rng: &mut R, n: usize) -> String {
    let mut pat: Vec<char> = rand_inp(rng, n).chars().collect();
    let n_wildcards = rng.gen_range(0, (0.75 * n as f32) as usize);
    for _ in 0..n_wildcards {
        let mut i = rng.gen_range(0, n);
        while pat[i] == '*' {
            i = rng.gen_range(0, n);
        }
        pat[i] = '*';
    }
    pat.into_iter().collect()
}

fn bench_obf_base(b: &mut Bencher, n: usize, secparam: usize) {
    let rng = &mut thread_rng();
    b.iter(|| {
        let pat = rand_pat(rng, n);
        let obf = WildcardObfuscation::encode(&pat, secparam);
        test::black_box(obf);
    });
}

fn bench_eval_base(b: &mut Bencher, n: usize, secparam: usize) {
    let rng = &mut thread_rng();
    let pat = rand_pat(rng, n);
    let obf = WildcardObfuscation::encode(&pat, secparam);
    b.iter(|| {
        let inp = rand_inp(rng, n);
        let res = obf.eval(&inp);
        test::black_box(res);
    });
}

#[bench] fn obf_s1024_n64 (b: &mut Bencher) { bench_obf_base(b, 64,  1024) }
#[bench] fn eval_s1024_n64 (b: &mut Bencher) { bench_eval_base(b, 64,  1024) }
