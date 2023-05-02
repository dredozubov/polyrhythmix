use std::str;
use std::vec::Vec;

pub use nom::character::complete::{char, digit1};
use nom::sequence::{separated_pair, tuple};
use nom::{Err, IResult};
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::{map, map_res};


#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BasicLength {
    Whole,
    Half,
    Fourth,
    Eighth,
    Sixteenth,
    ThirtySecond,
    SixtyFourth
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ModdedLength {
    Plain(BasicLength),
    Dotted(BasicLength)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Length {
    Simple(ModdedLength),
    Tied(ModdedLength, ModdedLength),
    Triplet(ModdedLength)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Note {
    Hit,
    Rest
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Group { notes: Vec<Note>, length: Length }

fn hit(input: &str) -> IResult<&str, Note> {
    //  note that this is really creating a function, the parser for abc
    //  vvvvv 
    //         which is then called here, returning an IResult<&str, &str>
    //         vvvvv
    map(char('x'), |_| { Note::Hit })(input)
}

fn rest(input: &str) -> IResult<&str, Note> {
    map(char('-'), |_| { Note::Rest })(input)
}

fn note(input: &str) -> IResult<&str, Note> {
    alt((hit, rest))(input)
}

fn length_basic(input: &str) -> IResult<&str, BasicLength> {
    match map_res(digit1, str::parse)(input) {
        Ok((r,1)) => Ok((r, BasicLength::Whole)),
        Ok((r,2)) => Ok((r, BasicLength::Half)),
        Ok((r,4)) => Ok((r, BasicLength::Fourth)),
        Ok((r,8)) => Ok((r, BasicLength::Eighth)),
        Ok((r,16)) => Ok((r, BasicLength::Sixteenth)),
        Ok((r,32)) => Ok((r, BasicLength::ThirtySecond)),
        Ok((r, 64)) => Ok((r, BasicLength::SixtyFourth)),
        Ok((r, i)) => {
            Err(Err::Error(nom::error::make_error(r, nom::error::ErrorKind::Fail)))
        },
        Err(e) => Err(e)
    }
}

fn dotted_length(input: &str) -> IResult<&str, ModdedLength> {
    map(tuple((length_basic, char('.'))), |(l, _)| { ModdedLength::Dotted(l)})(input)
}

fn modded_length(input: &str) -> IResult<&str, ModdedLength> {
    alt((dotted_length, map(length_basic, |x| {ModdedLength::Plain(x)})))(input)
}

fn triplet_length(input: &str) -> IResult<&str, Length> {
    map(tuple((modded_length, char('t'))), |(l, _)| { Length::Triplet(l)})(input)
}

fn tied_length(input: &str) -> IResult<&str, Length> {
    map(separated_pair(modded_length, char('+'), modded_length), |(x, y)| { Length::Tied(x,y)})(input)
}

fn length(input: &str) -> IResult<&str, Length> {
    alt((triplet_length, tied_length, map(modded_length, |x| { Length::Simple(x) })))(input)
}

#[test]
fn parse_basic_length() {
  assert_eq!(length("16"), Ok(("", Length::Simple(ModdedLength::Plain(BasicLength::Sixteenth)))));
  assert_eq!(length("8+16"), Ok(("", Length::Tied(ModdedLength::Plain(BasicLength::Eighth), ModdedLength::Plain(BasicLength::Sixteenth)))));
  assert_eq!(length("8t"), Ok(("", Length::Triplet(ModdedLength::Plain(BasicLength::Eighth)))));
  assert_eq!(length("4.t"), Ok(("", Length::Triplet(ModdedLength::Dotted(BasicLength::Fourth)))));
}

// “x” hit
// “-“ rest
// 16x-- => 16th hit and 16th rests

// - 16x-xx-x-8txxx(3,16+32x-xx)4x-x- => x-xx-x- of 16th, then three hits of 8th triplets, repeat a group of tied 16+32th x-xx three times, then 4th x-x