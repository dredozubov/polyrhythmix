use std::num::ParseIntError;
use std::ops::Add;
use std::str::{self, FromStr};
use std::vec::Vec;

use nom::branch::alt;
pub use nom::character::complete::{char, digit1};
use nom::multi::many1;
use nom::sequence::{delimited, separated_pair, tuple};
use nom::{Err, IResult};

use nom::combinator::{all_consuming, map, map_res};

/// Allows measurement in 128th notes.
pub trait KnownLength {
    fn to_128th(&self) -> u32;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum BasicLength {
    Whole,
    Half,
    Fourth,
    Eighth,
    Sixteenth,
    ThirtySecond,
    SixtyFourth,
}

impl FromStr for BasicLength {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let result: Result<u16, ParseIntError> = s.parse();
        match result {
            Ok(n) => Self::from_num(n),
            Result::Err(e) => panic!("{}", e),
        }
    }
}

impl KnownLength for BasicLength {
    fn to_128th(&self) -> u32 {
        match self {
            BasicLength::Whole => 128,
            BasicLength::Half => 64,
            BasicLength::Fourth => 32,
            BasicLength::Eighth => 16,
            BasicLength::Sixteenth => 8,
            BasicLength::ThirtySecond => 4,
            BasicLength::SixtyFourth => 2,
        }
    }
}

impl BasicLength {
    pub fn from_num(n: u16) -> Result<Self, String> {
        match n {
            64 => Ok(BasicLength::SixtyFourth),
            32 => Ok(BasicLength::ThirtySecond),
            16 => Ok(BasicLength::Sixteenth),
            8 => Ok(BasicLength::Eighth),
            4 => Ok(BasicLength::Fourth),
            2 => Ok(BasicLength::Half),
            1 => Ok(BasicLength::Whole),
            e => Err(format!(
                "{} is not a num BasicLength can be constructed from",
                e
            )),
        }
    }

    /// Private unsafe method, so it should only be used from `add`.
    /// Reverses `to_128th`.
    fn from_128th(n: u16) -> Result<Self, String> {
        match n {
            2 => Ok(BasicLength::SixtyFourth),
            4 => Ok(BasicLength::ThirtySecond),
            8 => Ok(BasicLength::Sixteenth),
            16 => Ok(BasicLength::Eighth),
            32 => Ok(BasicLength::Fourth),
            64 => Ok(BasicLength::Half),
            128 => Ok(BasicLength::Whole),
            e => Err(format!(
                "{} is not a num BasicLength can be constructed from",
                e
            )),
        }
    }
}

impl Add<BasicLength> for BasicLength {
    type Output = Length;

    fn add(self, rhs: BasicLength) -> Length {
        let f = |x| match x {
            BasicLength::Whole => 128,
            BasicLength::Half => 64,
            BasicLength::Fourth => 32,
            BasicLength::Eighth => 16,
            BasicLength::Sixteenth => 8,
            BasicLength::ThirtySecond => 4,
            BasicLength::SixtyFourth => 2,
        };
        if self == rhs && self != BasicLength::Whole {
            Length::Simple(ModdedLength::Plain(
                BasicLength::from_128th(f(self) * 2).unwrap(),
            ))
        } else {
            let n1: u16 = f(self);
            let n2 = f(rhs);
            let total = n1 + n2;

            if total > 128 {
                Length::Tied(
                    ModdedLength::Plain(BasicLength::Whole),
                    ModdedLength::Plain(BasicLength::from_128th(total - 128).unwrap()),
                )
            } else if total > 32 {
                Length::Tied(
                    ModdedLength::Plain(BasicLength::Half),
                    ModdedLength::Plain(BasicLength::from_128th(total - 64).unwrap()),
                )
            } else if total > 16 {
                Length::Tied(
                    ModdedLength::Plain(BasicLength::Fourth),
                    ModdedLength::Plain(BasicLength::from_128th(total - 32).unwrap()),
                )
            } else if total > 8 {
                Length::Tied(
                    ModdedLength::Plain(BasicLength::Eighth),
                    ModdedLength::Plain(BasicLength::from_128th(total - 16).unwrap()),
                )
            } else if total > 4 {
                Length::Tied(
                    ModdedLength::Plain(BasicLength::Fourth),
                    ModdedLength::Plain(BasicLength::from_128th(total - 8).unwrap()),
                )
            } else {
                Length::Tied(
                    ModdedLength::Plain(BasicLength::Half),
                    ModdedLength::Plain(BasicLength::from_128th(total - 4).unwrap()),
                )
            }
        }
    }
}

#[test]
fn test_add_basic_length() {
    assert_eq!(
        BasicLength::Half + BasicLength::Half,
        Length::Simple(ModdedLength::Plain(BasicLength::Whole))
    );
    assert_eq!(
        BasicLength::Whole + BasicLength::Whole,
        Length::Tied(
            ModdedLength::Plain(BasicLength::Whole),
            ModdedLength::Plain(BasicLength::Whole)
        )
    );
    assert_eq!(
        BasicLength::Half + BasicLength::SixtyFourth,
        Length::Tied(
            ModdedLength::Plain(BasicLength::Half),
            ModdedLength::Plain(BasicLength::SixtyFourth)
        )
    );
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ModdedLength {
    Plain(BasicLength),
    Dotted(BasicLength),
}

impl KnownLength for ModdedLength {
    fn to_128th(&self) -> u32 {
        match self {
            ModdedLength::Plain(bl) => bl.to_128th(),
            ModdedLength::Dotted(bl) => {
                let l = bl.to_128th();
                l + l / 2
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Length {
    Simple(ModdedLength),
    Tied(ModdedLength, ModdedLength),
    Triplet(ModdedLength),
}

impl KnownLength for Length {
    fn to_128th(&self) -> u32 {
        match self {
            Length::Simple(ml) => ml.to_128th(),
            Length::Tied(ml1, ml2) => ml1.to_128th() + ml2.to_128th(),
            Length::Triplet(ml) => ml.to_128th() * 2 / 3,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Note {
    Hit,
    Rest,
}

use Note::*;

#[allow(dead_code)]
pub(crate) static WHOLE: &Length = &Length::Simple(ModdedLength::Plain(BasicLength::Whole));
#[allow(dead_code)]
pub(crate) static HALF: &Length = &Length::Simple(ModdedLength::Plain(BasicLength::Half));
#[allow(dead_code)]
pub(crate) static FOURTH: &Length = &Length::Simple(ModdedLength::Plain(BasicLength::Fourth));
#[allow(dead_code)]
pub(crate) static EIGHTH: &Length = &Length::Simple(ModdedLength::Plain(BasicLength::Eighth));
#[allow(dead_code)]
pub(crate) static SIXTEENTH: &Length = &Length::Simple(ModdedLength::Plain(BasicLength::Sixteenth));
#[allow(dead_code)]
pub(crate) static THIRTY_SECOND: &Length =
    &Length::Simple(ModdedLength::Plain(BasicLength::ThirtySecond));
#[allow(dead_code)]
pub(crate) static SIXTY_FOURTH: &Length =
    &Length::Simple(ModdedLength::Plain(BasicLength::SixtyFourth));

#[allow(dead_code)]
pub(crate) static WHOLE_DOTTED_TRIPLET: &Length =
    &Length::Triplet(ModdedLength::Dotted(BasicLength::Whole));
#[allow(dead_code)]
pub(crate) static HALF_DOTTED_TRIPLET: &Length =
    &Length::Triplet(ModdedLength::Dotted(BasicLength::Half));
#[allow(dead_code)]
pub(crate) static FOURTH_DOTTED_TRIPLET: &Length =
    &Length::Triplet(ModdedLength::Dotted(BasicLength::Fourth));
#[allow(dead_code)]
pub(crate) static EIGHTH_DOTTED_TRIPLET: &Length =
    &Length::Triplet(ModdedLength::Dotted(BasicLength::Eighth));
#[allow(dead_code)]
pub(crate) static SIXTEENTH_DOTTED_TRIPLET: &Length =
    &Length::Triplet(ModdedLength::Dotted(BasicLength::Sixteenth));
#[allow(dead_code)]
pub(crate) static THIRTY_SECOND_DOTTED_TRIPLET: &Length =
    &Length::Triplet(ModdedLength::Dotted(BasicLength::ThirtySecond));
#[allow(dead_code)]
pub(crate) static SIXTY_FOURTH_DOTTED_TRIPLET: &Length =
    &Length::Triplet(ModdedLength::Dotted(BasicLength::SixtyFourth));

#[allow(dead_code)]
pub(crate) static WHOLE_TRIPLET: &Length =
    &Length::Triplet(ModdedLength::Plain(BasicLength::Whole));
#[allow(dead_code)]
pub(crate) static HALF_TRIPLET: &Length = &Length::Triplet(ModdedLength::Plain(BasicLength::Half));
#[allow(dead_code)]
pub(crate) static FOURTH_TRIPLET: &Length =
    &Length::Triplet(ModdedLength::Plain(BasicLength::Fourth));
#[allow(dead_code)]
pub(crate) static EIGHTH_TRIPLET: &Length =
    &Length::Triplet(ModdedLength::Plain(BasicLength::Eighth));
#[allow(dead_code)]
pub(crate) static SIXTEENTH_TRIPLET: &Length =
    &Length::Triplet(ModdedLength::Plain(BasicLength::Sixteenth));
#[allow(dead_code)]
pub(crate) static THIRTY_SECOND_TRIPLET: &Length =
    &Length::Triplet(ModdedLength::Plain(BasicLength::ThirtySecond));
#[allow(dead_code)]
pub(crate) static SIXTY_FOURTH_TRIPLET: &Length =
    &Length::Triplet(ModdedLength::Plain(BasicLength::SixtyFourth));

#[allow(dead_code)]
pub(crate) static HIT: GroupOrNote<Times> = GroupOrNote::SingleNote(Note::Hit);
#[allow(dead_code)]
pub(crate) static REST: GroupOrNote<Times> = GroupOrNote::SingleNote(Note::Rest);

#[allow(dead_code)]
pub(crate) static ONCE: &Times = &Times(1);
#[allow(dead_code)]
pub(crate) static TWICE: &Times = &Times(2);
#[allow(dead_code)]
pub(crate) static THRICE: &Times = &Times(3);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct Times(pub u16);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GroupOrNote<T> {
    SingleGroup(Group<GroupOrNote<T>, T>),
    SingleNote(Note),
}

use GroupOrNote::*;

/// There are two useful instantiations of this type:
/// `Group<GroupOrNote>` acts as a recursive `Group`, dsl parser uses this as return type
/// `Group<Note>` is a non-recursive group. To go from recursive groups to not-recursive ones, try using `flatten_group`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Group<T, R> {
    pub notes: Vec<T>,
    pub length: Length,
    pub times: R,
}

impl<T> Group<T, Times> {
    pub fn empty() -> Self {
        Group {
            notes: Vec::new(),
            length: FOURTH.clone(),
            times: Times(1),
        }
    }
}

impl KnownLength for &Group<GroupOrNote<Times>, Times> {
    fn to_128th(&self) -> u32 {
        let mut acc = 0;
        let note_length = self.length.to_128th();
        for group in self.notes.iter() {
            match group {
                GroupOrNote::SingleGroup(subgroup) => {
                    acc += subgroup.to_128th();
                }
                GroupOrNote::SingleNote(_) => {
                    acc += note_length;
                }
            }
        }
        acc * self.times.0 as u32
    }
}

impl KnownLength for Group<GroupOrNote<Times>, Times> {
    fn to_128th(&self) -> u32 {
        let mut acc = 0;
        let note_length = self.length.to_128th();
        for group in self.notes.iter() {
            match group {
                GroupOrNote::SingleGroup(subgroup) => {
                    acc += subgroup.to_128th();
                }
                GroupOrNote::SingleNote(_) => {
                    acc += note_length;
                }
            }
        }
        acc * self.times.0 as u32
    }
}

impl KnownLength for Group<Note, ()> {
    fn to_128th(&self) -> u32 {
        let mut acc = 0;
        let note_length = self.length.to_128th();
        for group in self.notes.iter() {
            acc += note_length;
        }
        acc
    }
}

#[test]
fn test_known_length_group() {
    let group = Group {
        notes: vec![
            SingleNote(Hit),
            SingleNote(Hit),
            SingleNote(Rest),
            SingleNote(Hit),
            SingleNote(Rest),
            SingleNote(Hit),
            SingleNote(Hit),
            SingleNote(Rest),
        ],
        length: SIXTEENTH.clone(),
        times: Times(1),
    };
    assert_eq!(group.to_128th(), 64);
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Groups(pub Vec<Group<Note, ()>>);

impl IntoIterator for Groups {
    type Item = Group<Note, ()>;

    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl FromIterator<Group<Note, ()>> for Groups {
    fn from_iter<T: IntoIterator<Item = Group<Note, ()>>>(iter: T) -> Self {
        Self(Vec::from_iter(iter))
    }
}

impl KnownLength for Groups {
    fn to_128th(&self) -> u32 {
        self.0.iter().fold(0, |acc, x| acc + x.to_128th())
    }
}

impl KnownLength for &Groups {
    fn to_128th(&self) -> u32 {
        self.0.iter().fold(0, |acc, x| acc + x.to_128th())
    }
}

#[test]
fn test_known_length_groups() {
    let groups = Groups(vec![Group {
        notes: vec![Hit, Hit, Rest, Hit, Rest, Hit, Hit, Rest],
        length: SIXTEENTH.clone(),
        times: (),
    }]);
    assert_eq!(groups.to_128th(), 64);
}

fn hit(input: &str) -> IResult<&str, Note> {
    map(char('x'), |_| Note::Hit)(input)
}

fn rest(input: &str) -> IResult<&str, Note> {
    map(char('-'), |_| Note::Rest)(input)
}

fn note(input: &str) -> IResult<&str, Note> {
    alt((hit, rest))(input)
}

fn length_basic(input: &str) -> IResult<&str, BasicLength> {
    match map_res(digit1, str::parse)(input) {
        Ok((r, 1)) => Ok((r, BasicLength::Whole)),
        Ok((r, 2)) => Ok((r, BasicLength::Half)),
        Ok((r, 4)) => Ok((r, BasicLength::Fourth)),
        Ok((r, 8)) => Ok((r, BasicLength::Eighth)),
        Ok((r, 16)) => Ok((r, BasicLength::Sixteenth)),
        Ok((r, 32)) => Ok((r, BasicLength::ThirtySecond)),
        Ok((r, 64)) => Ok((r, BasicLength::SixtyFourth)),
        Ok((r, _)) => Err(Err::Error(nom::error::make_error(
            r,
            nom::error::ErrorKind::Fail,
        ))),
        Err(e) => Err(e),
    }
}

fn dotted_length(input: &str) -> IResult<&str, ModdedLength> {
    map(tuple((length_basic, char('.'))), |(l, _)| {
        ModdedLength::Dotted(l)
    })(input)
}

fn modded_length(input: &str) -> IResult<&str, ModdedLength> {
    alt((dotted_length, map(length_basic, |x| ModdedLength::Plain(x))))(input)
}

fn triplet_length(input: &str) -> IResult<&str, Length> {
    map(tuple((modded_length, char('t'))), |(l, _)| {
        Length::Triplet(l)
    })(input)
}

fn tied_length(input: &str) -> IResult<&str, Length> {
    map(
        separated_pair(modded_length, char('+'), modded_length),
        |(x, y)| Length::Tied(x, y),
    )(input)
}

fn length(input: &str) -> IResult<&str, Length> {
    alt((
        triplet_length,
        tied_length,
        map(modded_length, |x| Length::Simple(x)),
    ))(input)
}

fn times(input: &str) -> IResult<&str, Times> {
    map(map_res(digit1, str::parse), |x| Times(x))(input)
}

fn group(input: &str) -> IResult<&str, Group<GroupOrNote<Times>, Times>> {
    let repeated_syntax = map(
        tuple((
            times,
            char(','),
            length,
            many1(alt((
                map_res(note, |x| -> Result<GroupOrNote<Times>, &str> { Ok(SingleNote(x))}),
                map_res(delimited_group, |x| -> Result<GroupOrNote<Times>, &str> { Ok(SingleGroup(x))}),
            ))),
        )),
        |(t, _, l, n)| (t, l, n),
    );
    let single_syntax = map(
        tuple((
            length,
            many1(alt((
                map_res(note, |x| -> Result<GroupOrNote<Times>, &str> { Ok(SingleNote(x))}),
                map_res(delimited_group, |x| -> Result<GroupOrNote<Times>, &str> { Ok(SingleGroup(x))}),
            ))),
        )), |(l, vn)| (Times(1), l, vn));
    let (rem, (t, l, n)) = alt((repeated_syntax, single_syntax))(input)?;
    Ok((
        rem,
        Group {
            notes: n
                .into_iter()
                .collect(),
            length: l,
            times: t,
        },
    ))
}

fn delimited_group(input: &str) -> IResult<&str, Group<GroupOrNote<Times>, Times>> {
    delimited(char('('), group, char(')'))(input)
}

pub fn group_or_delimited_group(input: &str) -> IResult<&str, Group<GroupOrNote<Times>, Times>> {
    alt((delimited_group, group))(input)
}

pub fn groups(input: &str) -> IResult<&str, Groups> {
    map_res(
        all_consuming(many1(group_or_delimited_group)),
        |gs| -> Result<Groups, &str> {
            Ok(flatten_groups(gs))
        })(input)
}

pub fn flatten_groups<I>(input_groups: I) -> Groups
where
    I: IntoIterator<Item = Group<GroupOrNote<Times>, Times>>,
{
    let mut out = Vec::new();
    input_groups.into_iter().for_each(|g| {
        let flattened = flatten_group(g).0;
        out.extend(flattened);
    });
    Groups(out)
}

pub fn flatten_group(input: Group<GroupOrNote<Times>, Times>) -> Groups {
    flatten_group_(&input, &mut Vec::new())
}

fn flatten_group_(input: &Group<GroupOrNote<Times>, Times>, out_groups: &mut Vec<Group<Note, ()>>) -> Groups {
    let mut note_group = Vec::new();
    let inlined_notes = input.notes.iter().cycle().take(input.notes.len() * input.times.0 as usize);
    let group = Group { notes: inlined_notes.collect(), times: (), length: input.length };
    group.notes.iter().for_each(|&g| {
        match g {
            SingleGroup(group) => {
                let isolated_group = Group {
                    notes: note_group.clone(),
                    length: input.length,
                    times: (),
                };
                out_groups.push(isolated_group);
                note_group.clear();
                flatten_group_(&group, out_groups);
            }
            SingleNote(note) => {
                note_group.push(*note);
            }
        }
    });
    if !note_group.is_empty() {
        let isolated_group = Group {
            notes: note_group.clone(),
            length: group.length,
            times: (),
        };
        out_groups.push(isolated_group);
    }
    Groups(out_groups.iter().cloned().collect())
}

#[test]
fn test_flatten_group() {
    let output = Groups(vec![
        Group { notes: vec![Hit], length: *SIXTEENTH, times: () },
        Group { notes: vec![Rest, Hit, Rest, Hit], length: *EIGHTH, times: () },
        Group { notes: vec![Hit], length: *SIXTEENTH, times: () },
        Group { notes: vec![Rest, Hit, Rest, Hit], length: *EIGHTH, times: () },
        Group { notes: vec![Hit], length: *SIXTEENTH, times: () },
        Group { notes: vec![Rest, Hit, Rest, Hit], length: *EIGHTH, times: () },
    ]);
    // basically it's 3,16x(2,8-x)
    let input = Group {
        notes: vec![
            SingleNote(Hit), SingleGroup(Group {
                notes: vec![SingleNote(Rest), SingleNote(Hit)],
                length: *EIGHTH,
                times: Times(2),
            })],
        length: *SIXTEENTH,
        times: Times(3),
    };
    assert_eq!(flatten_group(input), output);
}

#[test]
fn parse_length() {
    assert_eq!(length("16"), Ok(("", *SIXTEENTH)));
    assert_eq!(
        length("8+16"),
        Ok((
            "",
            Length::Tied(
                ModdedLength::Plain(BasicLength::Eighth),
                ModdedLength::Plain(BasicLength::Sixteenth)
            )
        ))
    );
    assert_eq!(length("8t"), Ok(("", *EIGHTH_TRIPLET)));
    assert_eq!(length("4.t"), Ok(("", *FOURTH_DOTTED_TRIPLET)));
}

#[test]
fn test_parse_groups() {
    assert_eq!(
        groups("8x-(7,8xx)"),
        Ok((
            "",
            Groups(vec![
                Group {
                    notes: vec![Hit, Rest],
                    length: *EIGHTH,
                    times: ()
                },
                Group {
                    notes: vec![Hit, Hit, Hit, Hit, Hit, Hit, Hit, Hit, Hit, Hit, Hit, Hit, Hit, Hit],
                    length: *EIGHTH,
                    times: ()
                }
            ])
        ))
    );
    assert_eq!(
        groups("8x-(7,8xx"),
        Err(Err::Error(nom::error::make_error(
            "(7,8xx",
            nom::error::ErrorKind::Eof
        )))
    );
}

#[test]
fn test_parse_group() {
    let expectation = Group {
        times: *TWICE,
        notes: vec![
            SingleNote(Hit),
            SingleGroup(Group {
                times: *ONCE,
                notes: vec![SingleNote(Rest), SingleNote(Hit)],
                length: *EIGHTH,
            }),
        ],
        length: *SIXTEENTH,
    };
    assert_eq!(group("2,16x(8-x)"), Ok(("", expectation)));
    assert_eq!(
        group("16x--x-"),
        Ok((
            "",
            Group {
                times: *ONCE,
                notes: vec![
                    HIT.clone(),
                    REST.clone(),
                    REST.clone(),
                    HIT.clone(),
                    REST.clone()
                ],
                length: *SIXTEENTH
            }
        ))
    );
    assert_eq!(
        group("8txxx"),
        Ok((
            "",
            Group {
                times: *ONCE,
                notes: vec![HIT.clone(), HIT.clone(), HIT.clone()],
                length: *EIGHTH_TRIPLET
            }
        ))
    );
    assert_eq!(
        group("16+32x-xx"),
        Ok((
            "",
            Group {
                times: *ONCE,
                notes: vec![HIT.clone(), REST.clone(), HIT.clone(), HIT.clone()],
                length: Length::Tied(
                    ModdedLength::Plain(BasicLength::Sixteenth),
                    ModdedLength::Plain(BasicLength::ThirtySecond)
                )
            }
        ))
    );
    assert_eq!(
        group("3,16xx"),
        Ok((
            "",
            Group {
                times: *THRICE,
                length: *SIXTEENTH,
                notes: vec![HIT.clone(), HIT.clone()]
            }
        ))
    );
}

#[test]
fn parse_delimited_group() {
    assert_eq!(
        delimited_group("(3,16x--x-)"),
        Ok((
            "",
            Group {
                times: *THRICE,
                notes: vec![
                    HIT.clone(),
                    REST.clone(),
                    REST.clone(),
                    HIT.clone(),
                    REST.clone()
                ],
                length: *SIXTEENTH
            }
        ))
    );
}

#[test]
fn test_parse_group_or_delimited_group() {
    assert_eq!(
        group_or_delimited_group("(3,16x--x-)"),
        Ok((
            "",
            Group {
                times: *THRICE,
                notes: vec![
                    HIT.clone(),
                    REST.clone(),
                    REST.clone(),
                    HIT.clone(),
                    REST.clone()
                ],
                length: *SIXTEENTH
            }
        ))
    );
    assert_eq!(
        group_or_delimited_group("16x--x-"),
        Ok((
            "",
            Group {
                times: *ONCE,
                notes: vec![
                    HIT.clone(),
                    REST.clone(),
                    REST.clone(),
                    HIT.clone(),
                    REST.clone()
                ],
                length: *SIXTEENTH
            }
        ))
    );
    assert_eq!(
        group_or_delimited_group("(7,8xx"),
        Err(Err::Error(nom::error::make_error(
            "(7,8xx",
            nom::error::ErrorKind::Digit
        )))
    );
}

// “x” hit
// “-“ rest
// 16x-- => 16th hit and 16th rests

// - 16x-xx-x-8txxx(3,16+32x-xx)4x-x- => x-xx-x- of 16th, then three hits of 8th triplets, repeat a group of tied 16+32th x-xx three times, then 4th x-x
