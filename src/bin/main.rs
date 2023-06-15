use std::collections::BTreeMap;
use std::process::exit;
use std::str::FromStr;

use poly::dsl::dsl::{self, KnownLength, flatten_groups};
use poly::midi::core::{create_smf, Part};
use poly::midi::time::TimeSignature;

use clap::*;

#[derive(Debug, Parser, Clone)]
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
    tempo: u16,

    #[arg(short = 's', default_value = "4/4")]
    time_signature: String,

    #[arg(short = 'o', default_value = None)]
    output: Option<String>,
}

fn part_to_string(part: Part) -> String {
    match part {
        Part::KickDrum => String::from("Kick Drum"),
        Part::SnareDrum => String::from("Snare Drum"),
        Part::HiHat => String::from("Hi-Hat"),
        Part::CrashCymbal => String::from("Crash Cymbal"),
    }
}

fn validate_and_parse_part(
    cli: Option<String>,
    part: Part,
    patterns: &mut BTreeMap<Part, dsl::Groups>,
) -> () {
    match cli {
        None => {}
        Some(pattern) => match dsl::groups(pattern.as_str()) {
            Ok((_, groups)) => {
                println!("{:?}: {:?}", part, groups);
                // println!("group to 128th: {}", group.to_128th());
                patterns.insert(part, groups);
            }
            Err(_) => {
                panic!("{} pattern is malformed.", part_to_string(part))
            }
        },
    }
}

fn create_text_description(kick: &Option<String>, snare: &Option<String>, hihat: &Option<String>, crash: &Option<String>) -> String {
    let mut parts: String = "".to_string();
    if kick.is_some() {
        parts.push_str(&format!("\nKick Drum - {}", kick.clone().unwrap()));
    }
    if snare.is_some() {
        parts.push_str(&format!("\nSnare Drum - {}", snare.clone().unwrap()));
    }
    if hihat.is_some() {
        parts.push_str(&format!("\nHi-Hat - {}", hihat.clone().unwrap()));
    }
    if crash.is_some() {
        parts.push_str(&format!("\nCrash Cymbal - {}", crash.clone().unwrap()));
    }
    format!("{}{}", "Created using Poly. Part blueprints:", parts)
}

fn main() {
    let matches = Cli::parse();
    match matches {
        Cli {
            kick,
            snare,
            hihat,
            crash,
            tempo,
            time_signature,
            output,
        } => {
            if kick == None && snare == None && hihat == None && crash == None {
                println!("No drum pattern was supplied, exiting...");
                exit(1)
            } else {
                let signature = match TimeSignature::from_str(&time_signature) {
                    Err(e) => panic!("Can't parse the time signature: {}", e),
                    Ok(x) => x,
                };
                let text_description = create_text_description(&kick, &snare, &hihat, &crash);

                let mut groups = BTreeMap::new();
                validate_and_parse_part(kick, Part::KickDrum, &mut groups);
                validate_and_parse_part(snare, Part::SnareDrum, &mut groups);
                validate_and_parse_part(hihat, Part::HiHat, &mut groups);
                validate_and_parse_part(crash, Part::CrashCymbal, &mut groups);

                let output_file = output.clone();

                match output_file {
                    None => {
                        println!("No output file path was supplied, running a dry run...");
                        create_smf(groups, signature, text_description.as_str(), tempo)
                    }
                    Some(path) => {
                        match create_smf(groups, signature, text_description.as_str(), tempo)
                            .save(path.clone())
                        {
                            Ok(_) => {
                                println!("{} was written successfully", path);
                                exit(0)
                            }
                            Err(e) => {
                                println!("Failed to write {}: {}", path, e);
                                exit(1)
                            }
                        };
                    }
                };
            }
        }
    }
}
