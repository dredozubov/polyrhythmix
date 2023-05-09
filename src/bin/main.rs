use std::collections::HashMap;
use std::process::exit;

use poly::dsl::dsl;
use poly::midi;
use poly::midi::core::Part;

use clap::*;


#[derive(Debug, Parser)]
#[command(name = "poly")]
#[command(author = "Denis Redozubov <denis.redozubov@gmail.com>")]
#[command(version = "0.1")]
#[command(about = "Polyrhythmically-inclinded Midi Drum generator", long_about = None)]
struct Cli {
    #[arg(short = 'k', default_value = None)]
    kick: Option<String>,

    #[arg(short = 's', default_value = None)]
    snare: Option<String>,

    #[arg(short = 'h', default_value = None)]
    hihat: Option<String>,

    #[arg(short = 'c', default_value = None)]
    crash: Option<String>,

    #[arg(short = 't', default_value = "120")]
    tempo: String,

    #[arg(short = 'S', default_value = "4/4")]
    time_signature: String,

    #[arg(short = 'o', default_value = None)]
    output: Option<String>
}

fn part_to_string(part: Part) -> String {
    match part {
        Part::KickDrum => String::from("Kick Drum"),
        Part::SnareDrum => String::from("Snare Drum"),
        Part::HiHat => String::from("Hi-Hat"),
        Part::CrashCymbal => String::from("Crash Cymbal"),
    }
}

fn validate_and_parse_part(cli: Option<String>, part: Part, patterns: &mut HashMap<Part, Vec<dsl::Group>>) -> () {
    match cli {
        None => {},
        Some(pattern) => {
            match dsl::groups(pattern.as_str()) {
                Ok((_, group)) => { patterns.insert(part, group); },
                Err(_) => { 
                    panic!("{} pattern is malformed.", part_to_string(part))
                }
            }    
        }
    }
}

fn main() {
    let matches = Cli::parse();
    let mut drum_patterns : HashMap<Part, Vec<dsl::Group>> = HashMap::new();
    match matches {
        Cli { kick, snare, hihat, crash, tempo, time_signature, output} => {
            if kick == None && snare == None && hihat == None && crash == None {
                println!("No drum pattern was supplied, exiting...");
                exit(1)
            } else if output == None {
                println!("No output file path was supplied, running a dry run...")
            } else {
                validate_and_parse_part(kick, Part::KickDrum, &mut drum_patterns);
                validate_and_parse_part(snare, Part::SnareDrum, &mut drum_patterns);
                validate_and_parse_part(hihat, Part::HiHat, &mut drum_patterns);
                validate_and_parse_part(crash, Part::CrashCymbal, &mut drum_patterns);

            }
        }
    }
}
