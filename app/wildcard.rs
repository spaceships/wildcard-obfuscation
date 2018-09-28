extern crate clap;
extern crate wildcard;

use wildcard::protocol::Obf;

use std::io::Read;
use clap::{Arg, App, SubCommand, AppSettings};
use std::process::exit;

fn main() {
    let matches = App::new("wildcard")
        .setting(AppSettings::SubcommandRequiredElseHelp)
        .global_setting(AppSettings::ColoredHelp)
        .about("BKLMPRS18 wildcard obfuscation scheme")

        .subcommand(SubCommand::with_name("obf")
                    .display_order(1)
                    .about("obfuscate a pattern")
                    .arg(Arg::with_name("PATTERN")
                         .help("the pattern to obfuscate (\"-\" to read from stdin)")
                         .required(true)
                         .index(1))
                    .arg(Arg::with_name("output_file")
                         .help("output the obfuscation to FILE")
                         .value_name("FILE")
                         .short("o")
                         .default_value("wildcard.obf"))
                    .arg(Arg::with_name("secparam")
                         .help("size of primes")
                         .value_name("NUM")
                         .default_value("1024")
                         .short("s")
                         .long("secparam")))

        .subcommand(SubCommand::with_name("multimatch")
                    .display_order(2)
                    .about("obfuscate alternative patterns")
                    .arg(Arg::with_name("PATTERNS")
                         .help("the patterns to obfuscate separated by whitespace (\"-\" to read from stdin)")
                         .required(true)
                         .index(1))
                    .arg(Arg::with_name("output_file")
                         .help("output the obfuscation to FILE")
                         .value_name("FILE")
                         .short("o")
                         .default_value("wildcard.obf"))
                    .arg(Arg::with_name("secparam")
                         .help("size of primes")
                         .value_name("NUM")
                         .default_value("1024")
                         .short("s")
                         .long("secparam")))

        .subcommand(SubCommand::with_name("eval")
                    .display_order(3)
                    .about("evaluate an obfuscated pattern on an input string")
                    .arg(Arg::with_name("obf_file")
                         .help("read the obfuscation from FILE")
                         .value_name("FILE")
                         .short("o")
                         .default_value("wildcard.obf"))
                    .arg(Arg::with_name("INPUT")
                         .help("the input string to evaluate (\"-\" to read from stdin)")
                         .required(true)
                         .index(1)))
        .get_matches();

    match matches.subcommand() {
        ("obf", Some(matches)) => {
            let pat = matches.value_of("PATTERN").expect("PATTERN required!");
            let output = matches.value_of("output_file").unwrap();
            let secparam = matches.value_of("secparam").unwrap().parse()
                .expect("integer security parameter expected!");
            obf_main(pat, output, secparam);
        }

        ("multimatch", Some(matches)) => {
            let pat = matches.value_of("PATTERNS").expect("PATTERNS required!");
            let output = matches.value_of("output_file").unwrap();
            let secparam = matches.value_of("secparam").unwrap().parse()
                .expect("integer security parameter expected!");
            multimatch_main(pat, output, secparam);
        }

        ("eval", Some(matches)) => {
            let obf = matches.value_of("obf_file").unwrap();
            let inp = matches.value_of("INPUT").expect("INPUT required!");
            eval_main(obf, inp);
        }
        _ => {}
    }
}

fn obf_main(pat_inp: &str, output_file: &str, secparam: usize) {
    let mut pat;
    if pat_inp == "-" {
        pat = String::new();
        let stdin = std::io::stdin();
        let mut handle = stdin.lock();
        handle.read_to_string(&mut pat).unwrap();
    } else {
        pat = pat_inp.to_string();
    }

    for c in pat.chars() {
        match c {
            '0' | '1' | '*' => {}
            _ => {
                eprintln!("Error: unknown pattern character \"{}\"!", c);
                exit(1);
            }
        }
    }

    let obf = Obf::encode(&pat, secparam);

    obf.to_file(output_file);
}

fn multimatch_main(pats_inp: &str, output_file: &str, secparam: usize) {
    let mut pat;
    if pats_inp == "-" {
        pat = String::new();
        let stdin = std::io::stdin();
        let mut handle = stdin.lock();
        handle.read_to_string(&mut pat).unwrap();
    } else {
        pat = pats_inp.to_string();
    }

    let mut cleaned_pat = String::new();
    for (i,c) in pat.chars().enumerate() {
        match c {
            '0' | '1' | '*' | '?' | '(' | ')' | '|' => cleaned_pat.push(c),
            '\n' | '\r' | ' ' => {}
            _ => {
                eprintln!("Error: unknown pattern character at {}/{}", i, pat.len());
                exit(1);
            }
        }
    }

    let obf = Obf::multimatch(&cleaned_pat, secparam);

    obf.to_file(output_file);
}


fn eval_main(obf_file: &str, inp: &str) {
    for c in inp.chars() {
        match c {
            '0' | '1' => {}
            _ => {
                eprintln!("Error: unknown input character \"{}\"!", c);
                exit(1);
            }
        }
    }

    let obf = Obf::from_file(obf_file);

    let result = obf.eval(inp);

    println!("{}", result);
}


