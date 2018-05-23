extern crate gmp;
extern crate rand;
extern crate num;

pub mod protocol;
pub mod rand_mpz;

use protocol::WildcardObfuscation;

fn main() {
    let obf = WildcardObfuscation::encode("00*00*", 128);
    let result = obf.eval("001001");

    println!("{}", result);
}
