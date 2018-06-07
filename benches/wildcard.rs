#[macro_use]
extern crate criterion;

extern crate wildcard;
extern crate rand;

use wildcard::protocol::*;

use criterion::Criterion;
use rand::{thread_rng, Rng};
use std::char::from_digit;
use std::time::Duration;

const N: usize = 16;

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

fn bench_obf(c: &mut Criterion) {
    let secparam = 1024;
    c.bench_function("obf", move |b| {
        let rng = &mut thread_rng();
        b.iter(|| {
            let pat = rand_pat(rng, N);
            let obf = WildcardObfuscation::encode(&pat, secparam);
            criterion::black_box(obf);
        });
    });
}

fn bench_eval(c: &mut Criterion) {
    let secparam = 1024;
    c.bench_function("eval", move |b| {
        let rng = &mut thread_rng();
        let pat = rand_pat(rng, N);
        let obf = WildcardObfuscation::encode(&pat, secparam);
        b.iter(|| {
            let inp = rand_inp(rng, N);
            let res = obf.eval(&inp);
            criterion::black_box(res);
        });
    });
}

criterion_group!{
    name = benches;
    config = Criterion::default().sample_size(10).warm_up_time(Duration::from_millis(10));
    targets = bench_obf, bench_eval
}
criterion_main!(benches);
