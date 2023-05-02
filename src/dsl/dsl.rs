use std::str;
use std::vec::Vec;

pub use nom::character::complete::{char, digit1};
use nom::{Err, IResult};
use nom::branch::alt;
use nom::bytes::complete::tag;


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
    Dotted(BasicLength),
    Triplet(BasicLength)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Length {
    Simple(ModdedLength),
    Tied(ModdedLength, ModdedLength)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Note {
    Hit(Length),
    Rest(Length)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Group { notes: Vec<Note> }

fn hit(len: Length, input: &str) -> IResult<&str, Note> {
    //  note that this is really creating a function, the parser for abc
    //  vvvvv 
    //         which is then called here, returning an IResult<&str, &str>
    //         vvvvv
    let (rem, c) = char('x')(input)?;
    Ok((rem, Note::Hit(len)))
}

fn rest( len: Length, input: &str) -> IResult<&str, Note> {
    let (rem, c) = char('-')(input)?;
    Ok((rem, Note::Rest(len)))
}

fn note(input: &str, len: Length) -> IResult<&str, Note> {
    alt((hit(len), rest(len)))(input)
}

fn length_basic(input: &str) -> IResult<&str, Length> {
    match map_res(digit1, str::parse)(input) {
        Ok(r,1) => Ok(r, Whole),
        Ok(r,2) => Ok(r, Half),
        Ok(r,4) => Ok(r, Fourth),
        Ok(r,8) => Ok(r, Eighth),
        Ok(r,16) => Ok(r, Sixteenth),
        Ok(r,32) => Ok(r, ThirtySecond),
        Ok(r, 64) => Ok(r, SixtyFourth),
        e => e
    }
}

fn tie_length(input: &str) -> IResult<&str, Length> {
    let (rem, (l1,l2)) = separated_pair(length_basic, char('+'), length_basic)?;
    Ok(rem, Tie(l1, l2))
}

fn length(input: &str) -> IResult<&str, Length> {
    let (rem, len) = alt(tie_length, length_basic)(input)?;
    let (rem2, is_triplet) = char('t')(rem)?;
    if is_triplet {
        Triplet(len)
    } else {
        leṇ
    }
}

#[test]
fn parse_basic_length() {
  assert_eq!(
    length("16"),
    Ok((
      "",
      Sixteenth
    ))
  );
  assert_eq!(
    length("8+16"),
    Ok("", Tie(Eighth, Sixteenth))
  );
  assert_eq!(length("8t"), Triplet(Eighth))
}

// “x” hit
// “-“ rest
// 16x-- => 16th hit and 16th rests

// - 16x-xx-x-8txxx(3,16+32x-xx)4x-x- => x-xx-x- of 16th, then three hits of 8th triplets, repeat a group of tied 16+32th x-xx three times, then 4th x-x