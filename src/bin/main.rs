use std::collections::HashMap;
use std::process::exit;
use std::str::FromStr;

use poly::dsl::dsl;
use poly::midi::core::{Part, create_smf};
use poly::midi::time::TimeSignature;

use clap::*;


#[derive(Debug, Parser)]
#[command(name = "poly")]
#[command(author = "Denis Redozubov <denis.redozubov@gmail.com>")]
#[command(version = "0.1")]
#[command(about = "Polyrhythmically-inclinded Midi Drum generator", long_about = None)]
struct Cli {
    #[arg(short = 'K', default_value = None)]
    kick: Option<String>,

    #[arg(short = 'S', default_value = None)]
    snare: Option<String>,

    #[arg(short = 'H', default_value = None)]
    hihat: Option<String>,

    #[arg(short = 'C', default_value = None)]
    crash: Option<String>,

    #[arg(short = 't', default_value = "120")]
    tempo: String,

    #[arg(short = 's', default_value = "4/4")]
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
    match matches {
        Cli { kick, snare, hihat, crash, tempo, time_signature, output} => {
            if kick == None && snare == None && hihat == None && crash == None {
                println!("No drum pattern was supplied, exiting...");
                exit(1) 
            } else {
                let mut groups = HashMap::new();
                validate_and_parse_part(kick, Part::KickDrum, &mut groups);
                validate_and_parse_part(snare, Part::SnareDrum, &mut groups);
                validate_and_parse_part(hihat, Part::HiHat, &mut groups);
                validate_and_parse_part(crash, Part::CrashCymbal, &mut groups);

                let signature = match TimeSignature::from_str(&time_signature) {
                    Err(e) => panic!("Can't parse the time signature: {}", e),
                    Ok(x) => x
                };

                match output {
                    None => {
                        println!("No output file path was supplied, running a dry run...");
                        create_smf(groups, signature)
                    },
                    Some(path) => {
                        match create_smf(groups, signature).save(path.clone()) {
                            Ok(_) => {
                                println!("{} was written successfully", path);
                                exit(0)
                            },
                            Err(e) => {
                                println!("Failed to write {}: {}", path, e);
                                exit(1)
                            },
                        };
                    }
                };
            }
        }
    }
}
