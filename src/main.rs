extern crate gmp;
extern crate rand;
extern crate num;

pub mod protocol;
pub mod rand_mpz;

use protocol::WildcardObfuscation;

fn main() {
    let obf = WildcardObfuscation::encode("00000*", 16);
    let result = obf.eval("000001");

    println!("{}", result);
}

