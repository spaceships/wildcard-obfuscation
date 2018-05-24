extern crate clap;
extern crate wildcard;

use wildcard::protocol::WildcardObfuscation;

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
                         .help("security parameter")
                         .value_name("NUM")
                         .default_value("128")
                         .short("s")
                         .long("secparam")))

        .subcommand(SubCommand::with_name("eval")
                    .display_order(2)
                    .about("evaluate an obfuscated pattern on an input string")
                    .arg(Arg::with_name("OBF")
                         .help("read the obfuscation from OBF")
                         .required(true)
                         .index(1))
                    .arg(Arg::with_name("INPUT")
                         .help("the input string to evaluate (\"-\" to read from stdin)")
                         .required(true)
                         .index(2)))
        .get_matches();

    match matches.subcommand() {
        ("obf", Some(matches)) => {
            let pat = matches.value_of("PATTERN").expect("PATTERN required!");
            let output = matches.value_of("output_file").unwrap();
            let secparam = matches.value_of("secparam").unwrap().parse()
                .expect("integer security parameter expected!");
            obf_main(pat, output, secparam);
        }

        ("eval", Some(matches)) => {
            let obf = matches.value_of("OBF").expect("OBF required!");
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

    let obf = WildcardObfuscation::encode(&pat, secparam);

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

    let obf = WildcardObfuscation::from_file(obf_file);

    let result = obf.eval(inp);

    println!("{}", result);
}


