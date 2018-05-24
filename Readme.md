wildcard pattern matching obfuscation
=====================================

Implementation of the [BKLMPRS18](https://eprint.iacr.org/2018/210) wildcard pattern
matching obfuscation scheme. Currently it runs over the integers.

This is research code: use it how you like.

running
-------

`cargo bench` to run benchmarks

`cargo run -- obf 0111****111` to obfuscate the pattern `0111****111`. This generates the
file `wildcard.obf`. Then to evaluate on input `011110000111`, run `cargo run -- eval
wildcard.obf 011110000111`.

There are other commands. See `cargo run -- -h`.
