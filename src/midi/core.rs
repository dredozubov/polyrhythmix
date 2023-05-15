extern crate derive_more;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::iter::Cycle;
use std::iter::Peekable;
use std::ops::{Add, Mul};
use std::path::Iter;

use midly::{
    num::u15, num::u24, num::u28, num::u4, num::u7, Header, MidiMessage, Smf, Track, TrackEventKind,
};
use midly::{EventIter, MetaMessage, TrackEvent};

use crate::dsl::dsl::{
    group_or_delimited_group, groups, BasicLength, Group, GroupOrNote, Length, ModdedLength, Note,
    Times,
};

// Typically used as number of ticks since the beginning of the track.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    derive_more::Add,
    derive_more::Sub,
    derive_more::Mul,
    derive_more::Rem,
    derive_more::Display,
)]
#[repr(transparent)]
pub struct Tick(pub u128);

#[test]
fn test_add_tick() {
    assert_eq!(Tick(2) + Tick(2), Tick(4));
}

// Delta in time since the last MIDI event, measured in Ticks.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, derive_more::Add, derive_more::Mul,
)]
#[repr(transparent)]
pub struct Delta(pub u128);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum EventType {
    NoteOn(Part),
    NoteOff(Part),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct TimeSignature {
    pub numerator: u8,
    pub denominator: BasicLength,
}

impl TimeSignature {
    pub fn new(numerator: u8, denominator: BasicLength) -> Self {
        Self {
            numerator,
            denominator,
        }
    }
}

impl std::ops::Mul<u8> for TimeSignature {
    type Output = TimeSignature;
    fn mul(self, rhs: u8) -> TimeSignature {
        TimeSignature {
            numerator: self.numerator * rhs as u8,
            denominator: self.denominator,
        }
    }
}

#[test]
fn test_cmp_time_signature() {
    let three_sixteenth = TimeSignature {
        numerator: 3,
        denominator: BasicLength::Sixteenth,
    };
    let four_fourth = TimeSignature {
        numerator: 4,
        denominator: BasicLength::Fourth,
    };
    let two_secondth = TimeSignature {
        numerator: 2,
        denominator: BasicLength::Half,
    };
    assert_eq!(three_sixteenth.cmp(&four_fourth), Ordering::Less);
    // weird, but not worth changing
    // May implement a new type Ord if it needs to be Equal.
    assert_eq!(four_fourth.cmp(&two_secondth), Ordering::Greater);
}

impl TimeSignature {
    /// Checks if these two signatures converges for the next 200 bars.
    fn converges_with(&self, other: TimeSignature) -> Result<(u32, TimeSignature), String> {
        let d: u32 = std::cmp::max(self.denominator, other.denominator)
            .to_u8()
            .into();
        let d1: u32 = self.denominator.to_u8().into();
        let d2: u32 = other.denominator.to_u8().into();
        let coef1 = d / d1;
        let coef2 = d / d2;
        let num1: u32 = coef1 * (self.numerator as u32);
        let num2: u32 = coef2 * (other.numerator as u32);
        let greater_time_signature = self.max(&other);
        let f = |max, min| {
            let mut res = Err(format!("Not converges over 1000 bars of {:?}", other));
            for i in 1..1000 {
                if (max * i) % min == 0 {
                    res = Ok((i, *greater_time_signature));
                    break;
                }
            }
            res
        };
        match num1.cmp(&num2) {
            std::cmp::Ordering::Less => f(num2, num1),
            std::cmp::Ordering::Equal => Ok((1, *greater_time_signature)),
            std::cmp::Ordering::Greater => f(num1, num2),
        }
    }

    fn converges(
        &self,
        time_signatures: Vec<TimeSignature>,
    ) -> Result<(u32, TimeSignature), String> {
        time_signatures
            .iter()
            .try_fold((1, *self), |(bars, ts), x| match ts.converges_with(*x) {
                Ok((new_bars, greater_signature)) => {
                    if new_bars > bars {
                        if new_bars % bars == 0 {
                            Ok((new_bars, greater_signature))
                        } else {
                            Err(format!("{:?} don't converge with {:?}", self, x))
                        }
                    } else {
                        if bars % new_bars == 0 {
                            Ok((bars, greater_signature))
                        } else {
                            Err(format!("{:?} don't converge with {:?}", self, x))
                        }
                    }
                }
                Err(e) => Err(e),
            })
    }
}

#[test]
fn test_converges_with() {
    let three_sixteenth = TimeSignature {
        numerator: 3,
        denominator: BasicLength::Sixteenth,
    };
    let four_fourth = TimeSignature {
        numerator: 4,
        denominator: BasicLength::Fourth,
    };
    assert_eq!(
        three_sixteenth.converges_with(four_fourth),
        Ok((3, four_fourth))
    );
}

#[test]
fn test_converges() {
    let three_sixteenth = TimeSignature {
        numerator: 3,
        denominator: BasicLength::Sixteenth,
    };
    let four_fourth = TimeSignature {
        numerator: 4,
        denominator: BasicLength::Fourth,
    };
    let three_fourth = TimeSignature {
        numerator: 3,
        denominator: BasicLength::Fourth,
    };
    assert_eq!(
        three_sixteenth.converges(vec![four_fourth, three_fourth, four_fourth]),
        Ok((3, four_fourth))
    );
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub enum Part {
    KickDrum,
    SnareDrum,
    HiHat,
    CrashCymbal,
}

impl Part {
    // https://computermusicresource.com/GM.Percussion.KeyMap.html
    fn to_midi_key(&self) -> u7 {
        match self {
            Part::KickDrum => u7::from(36),
            Part::SnareDrum => u7::from(38),
            Part::HiHat => u7::from(46),
            Part::CrashCymbal => u7::from(49),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Event<T> {
    tick: T,
    event_type: EventType,
}

// Events are supposed to be sorted by T at all times.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct EventGrid<T> {
    events: Vec<Event<T>>,
    length: Tick,
}

impl<T> IntoIterator for EventGrid<T> {
    type Item = Event<T>;
    type IntoIter = std::vec::IntoIter<Event<T>>;

    fn into_iter(self) -> Self::IntoIter {
        self.events.into_iter()
    }
}

impl<T> EventGrid<T> {
    pub fn iter(&self) -> std::slice::Iter<'_, Event<T>> {
        self.events.iter()
    }
}

// impl <T> Iterator for EventGrid<T> {
//     type Item = Event<T>;

//     fn next(&mut self) -> Option<Self::Item> {
//         let e = self.events.iter().next();
//         e.map(|x| *x )
//     }
// }

impl<T: Add<Tick, Output = T> + Clone + Ord> Add for EventGrid<T> {
    type Output = EventGrid<T>;

    fn add(mut self, other: EventGrid<T>) -> EventGrid<T> {
        let other_events: Vec<Event<T>> = other
            .events
            .into_iter()
            .map(|mut e| {
                e.tick = e.tick.clone() + self.length;
                e
            })
            .collect();
        self.events.extend(other_events);
        self.events.sort();
        self.length = self.length + other.length;
        self
    }
}

impl<T: Clone + Ord> Mul for EventGrid<T> {
    type Output = EventGrid<T>;

    fn mul(mut self, other: EventGrid<T>) -> EventGrid<T> {
        let other_events: Vec<Event<T>> = other.events;

        self.events.extend(other_events);
        self.events.sort();
        self.length = self.length + other.length;
        self
    }
}

#[test]
fn test_arith_event_grids() {
    let eg1 = EventGrid {
        events: vec![
            Event {
                tick: Tick(0),
                event_type: EventType::NoteOn(Part::KickDrum),
            },
            Event {
                tick: Tick(TICKS_PER_QUARTER_NOTE as u128),
                event_type: EventType::NoteOff(Part::KickDrum),
            },
        ],
        length: Tick(TICKS_PER_QUARTER_NOTE as u128),
    };
    let eg2 = EventGrid {
        events: vec![
            Event {
                tick: Tick(24),
                event_type: EventType::NoteOn(Part::HiHat),
            },
            Event {
                tick: Tick(TICKS_PER_QUARTER_NOTE as u128),
                event_type: EventType::NoteOff(Part::HiHat),
            },
        ],
        length: Tick(TICKS_PER_QUARTER_NOTE as u128),
    };
    let mul_res = EventGrid {
        events: vec![
            Event {
                tick: Tick(0),
                event_type: EventType::NoteOn(Part::KickDrum),
            },
            Event {
                tick: Tick(24),
                event_type: EventType::NoteOn(Part::HiHat),
            },
            Event {
                tick: Tick(48),
                event_type: EventType::NoteOff(Part::KickDrum),
            },
            Event {
                tick: Tick(48),
                event_type: EventType::NoteOff(Part::HiHat),
            },
        ],
        length: Tick(96),
    };

    assert_eq!(eg1.clone() * eg2.clone(), mul_res);
}

#[test]
fn test_add_event_grid() {
    let mut empty: EventGrid<Tick> = EventGrid::new();
    let kick_on = Event {
        tick: Tick(0),
        event_type: EventType::NoteOn(Part::KickDrum),
    };
    let kick_off = Event {
        tick: Tick(24),
        event_type: EventType::NoteOff(Part::KickDrum),
    };
    let simple_grid = EventGrid {
        events: vec![kick_on, kick_off],
        length: Tick(48),
    };
    assert_eq!(empty.clone() + empty.clone(), empty);
    assert_eq!(simple_grid.clone() + empty.clone(), simple_grid);
    assert_eq!(empty.clone() + simple_grid.clone(), simple_grid);
    assert_eq!(
        simple_grid.clone() + simple_grid.clone(),
        EventGrid {
            events: vec![
                Event {
                    tick: Tick(0),
                    event_type: EventType::NoteOn(Part::KickDrum)
                },
                Event {
                    tick: Tick(24),
                    event_type: EventType::NoteOff(Part::KickDrum)
                },
                Event {
                    tick: Tick(48),
                    event_type: EventType::NoteOn(Part::KickDrum)
                },
                Event {
                    tick: Tick(72),
                    event_type: EventType::NoteOff(Part::KickDrum)
                }
            ],
            length: Tick(96)
        }
    );
}

impl<T> EventGrid<T> {
    fn new() -> Self {
        EventGrid {
            events: Vec::new(),
            length: Tick(0),
        }
    }
}

impl EventGrid<Tick> {
    /// Converts a sorted `EventGrid<Tick>`
    fn to_delta(&self) -> EventGrid<Delta> {
        let mut time = Tick(0);
        let mut delta_grid = EventGrid::new();
        for e in &self.events {
            let delta = e.tick - time;
            time = time + delta;
            delta_grid.events.push(Event {
                tick: Delta(delta.0),
                event_type: e.event_type,
            })
        }
        delta_grid
    }
}

#[allow(dead_code)]
static TICKS_PER_QUARTER_NOTE: u16 = 48;

impl BasicLength {
    /// `BasicLength` to MIDI Ticks
    fn to_ticks(&self) -> Tick {
        match self {
            BasicLength::Whole => Tick((TICKS_PER_QUARTER_NOTE * 4) as u128),
            BasicLength::Half => Tick((TICKS_PER_QUARTER_NOTE * 2) as u128),
            BasicLength::Fourth => Tick(TICKS_PER_QUARTER_NOTE as u128),
            BasicLength::Eighth => Tick((TICKS_PER_QUARTER_NOTE / 2) as u128),
            BasicLength::Sixteenth => Tick((TICKS_PER_QUARTER_NOTE / 4) as u128),
            BasicLength::ThirtySecond => Tick((TICKS_PER_QUARTER_NOTE / 8) as u128),
            BasicLength::SixtyFourth => Tick((TICKS_PER_QUARTER_NOTE / 16) as u128),
        }
    }

    fn to_u8(&self) -> u8 {
        match self {
            BasicLength::Whole => 1,
            BasicLength::Half => 2,
            BasicLength::Fourth => 4,
            BasicLength::Eighth => 8,
            BasicLength::Sixteenth => 16,
            BasicLength::ThirtySecond => 32,
            BasicLength::SixtyFourth => 64,
        }
    }
}

impl ModdedLength {
    /// `ModdedLength` to MIDI Ticks
    fn to_ticks(&self) -> Tick {
        match self {
            ModdedLength::Plain(blen) => blen.to_ticks(),
            ModdedLength::Dotted(blen) => {
                let Tick(whole) = blen.to_ticks();
                let half = whole / 2;
                Tick(whole + half)
            }
        }
    }
}

impl Length {
    /// Note length to MIDI ticks
    /// The function converts a musical note length to ticks, accounting for simple notes, tied notes, and
    /// triplets.
    ///
    /// Arguments:
    ///
    /// * `length`: `length` is a variable of type `Length`, which is an enum that represents different
    /// types of musical note lengths. The function `length_to_ticks` takes a `Length` as input and returns
    /// a `Tick`, which is a struct representing the number of ticks (a unit of time in music
    ///
    /// Returns:
    ///
    /// The function `length_to_ticks` takes a `Length` enum as input and returns a `Tick` value. The `Tick`
    /// value represents the duration of the note in ticks, which is a unit of time used in music notation
    /// software.
    fn to_ticks(&self) -> Tick {
        match self {
            Length::Simple(mlen) => mlen.to_ticks(),
            Length::Tied(first, second) => first.to_ticks() + second.to_ticks(),
            Length::Triplet(mlen) => {
                let Tick(straight) = mlen.to_ticks();
                let triplet = straight * 2 / 3;
                Tick(triplet)
            }
        }
    }
}

#[allow(dead_code)]
static MICROSECONDS_PER_BPM: u128 = 500000 as u128 / TICKS_PER_QUARTER_NOTE as u128;

#[allow(dead_code)]
static MIDI_CLOCKS_PER_CLICK: u8 = 24;

/// Microseconds per quarter note. Default is 500,000 for 120bpm.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    derive_more::Add,
    derive_more::Sub,
    derive_more::Mul,
    derive_more::Display,
)]
pub struct MidiTempo(u24);

// impl MidiTempo {
//     fn from_tempo(Tempo(t): Tempo) -> Self {
//         let mt = t as u32 * MICROSECONDS_PER_BPM as u32;
//         Self(u24::from(mt))
//     }
// }

/// Returns an EventGrid and a total length. Length is needed as a group can end with rests that are not in the grid,
/// and we need it to cycle the group.
fn flatten_group(
    Group {
        notes,
        length,
        times,
    }: &Group,
    part: Part,
    start: &mut Tick,
) -> EventGrid<Tick> {
    let time = start;
    let note_length = length.to_ticks();
    let mut grid = EventGrid::new();
    notes.iter().for_each(|entry| {
        match entry {
            crate::dsl::dsl::GroupOrNote::SingleGroup(group) => {
                let mut eg = flatten_group(&group, part, time);
                grid.events.append(&mut eg.events);
                grid.length = grid.length + eg.length;
            }
            crate::dsl::dsl::GroupOrNote::SingleNote(Note::Rest) => {
                let rest_end = *time + note_length;
                *time = rest_end;
                grid.length = rest_end;
            }
            crate::dsl::dsl::GroupOrNote::SingleNote(Note::Hit) => {
                let note_end = *time + note_length;
                let note_on = Event {
                    tick: *time,
                    event_type: EventType::NoteOn(part),
                };
                let note_off = Event {
                    tick: note_end,
                    event_type: EventType::NoteOff(part),
                };
                grid.events.push(note_on);
                grid.events.push(note_off);
                grid.length = note_end;
                *time = note_end;
            }
        };
    });
    cycle_grid(grid, *times)
}

#[test]
fn test_flatten_group() {
    assert_eq!(
        flatten_group(
            &group_or_delimited_group("(2,8x--)").unwrap().1,
            Part::KickDrum,
            &mut Tick(0)
        ),
        EventGrid {
            events: vec![
                Event {
                    tick: Tick(0),
                    event_type: EventType::NoteOn(Part::KickDrum)
                },
                Event {
                    tick: Tick(24),
                    event_type: EventType::NoteOff(Part::KickDrum)
                },
                Event {
                    tick: Tick(72),
                    event_type: EventType::NoteOn(Part::KickDrum)
                },
                Event {
                    tick: Tick(96),
                    event_type: EventType::NoteOff(Part::KickDrum)
                }
            ],
            length: Tick(144)
        }
    );
}

fn cycle_grid(event_grid: EventGrid<Tick>, times: Times) -> EventGrid<Tick> {
    let mut grid = EventGrid::new();
    for _ in 1..(times.0 + 1) {
        grid = grid + event_grid.clone();
    }
    grid
}

#[test]
fn test_cycle_grid() {
    let empty: EventGrid<Tick> = EventGrid::new();
    assert_eq!(cycle_grid(EventGrid::new(), Times(2)), empty);
    let kick_on = Event {
        tick: Tick(0),
        event_type: EventType::NoteOn(Part::KickDrum),
    };
    let kick_off = Event {
        tick: Tick(24),
        event_type: EventType::NoteOff(Part::KickDrum),
    };
    let simple_grid = EventGrid {
        events: vec![kick_on, kick_off],
        length: Tick(48),
    };
    assert_eq!(cycle_grid(simple_grid.clone(), Times(0)), empty);
    assert_eq!(cycle_grid(simple_grid.clone(), Times(1)), simple_grid);
    assert_eq!(
        cycle_grid(simple_grid.clone(), Times(2)),
        EventGrid {
            events: vec![
                Event {
                    tick: Tick(0),
                    event_type: EventType::NoteOn(Part::KickDrum)
                },
                Event {
                    tick: Tick(24),
                    event_type: EventType::NoteOff(Part::KickDrum)
                },
                Event {
                    tick: Tick(48),
                    event_type: EventType::NoteOn(Part::KickDrum)
                },
                Event {
                    tick: Tick(72),
                    event_type: EventType::NoteOff(Part::KickDrum)
                }
            ],
            length: Tick(96)
        }
    );
}

fn flatten_groups(part: Part, groups: &Vec<Group>) -> EventGrid<Tick> {
    let mut time: Tick = Tick(0);
    let mut grid: EventGrid<Tick> = EventGrid::new();
    groups.iter().for_each(|group| {
        grid = grid.clone() + flatten_group(group, part, &mut time);
    });
    grid
}

// Combines a vector of sorted EventGrid<Tick> into a single `EventGrid<Tick>`
fn merge_event_grids(mut eg: Vec<EventGrid<Tick>>) -> EventGrid<Tick> {
    let first = eg.pop().unwrap();
    eg.iter().fold(first, |mut acc, next| {
        acc = acc * (*next).clone();
        acc
    })
}

pub struct EventIterator<T>
where
    T:Clone
{
    kick: Peekable<Cycle<std::vec::IntoIter<Event<T>>>>,
    snare: Peekable<Cycle<std::vec::IntoIter<Event<T>>>>,
    hihat: Peekable<Cycle<std::vec::IntoIter<Event<T>>>>,
    crash: Peekable<Cycle<std::vec::IntoIter<Event<T>>>>,
    kick_length: Tick,
    snare_length: Tick,
    hihat_length: Tick,
    crash_length: Tick,
    limit: Tick,
    time: Tick
}

impl<T> EventIterator<T>
where
    T: Clone
 {
    fn new(
        kick_grid: EventGrid<T>,
        snare_grid: EventGrid<T>,
        hihat_grid: EventGrid<T>,
        crash_grid: EventGrid<T>,
        limit_value: Tick,
    ) -> EventIterator<T> {
        let event_iterator = EventIterator {
            kick_length: kick_grid.length.clone(),
            snare_length: snare_grid.length.clone(),
            hihat_length: hihat_grid.length.clone(),
            crash_length: crash_grid.length.clone(),
            kick: kick_grid.into_iter().cycle().peekable(),
            snare: snare_grid.into_iter().cycle().peekable(),
            hihat: hihat_grid.into_iter().cycle().peekable(),
            crash: crash_grid.into_iter().cycle().peekable(),
            limit: limit_value,
            time: Tick(0)
        };
        event_iterator
    }
}

impl Iterator for EventIterator<Tick> {
    type Item = Event<Tick>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut min_part = Part::KickDrum;
        let mut min_tick = self.limit;
        let mut min_event: Event<Tick> = Event { tick: Tick(0), event_type: EventType::NoteOn(Part::KickDrum) };
        let mut min_group_length: Tick;
        let candidates = vec![
            (self.kick.peek().unwrap(), Part::KickDrum),
            (self.snare.peek().unwrap(), Part::SnareDrum),
            (self.hihat.peek().unwrap(), Part::HiHat),
            (self.crash.peek().unwrap(), Part::CrashCymbal),
        ];

        for (&e, p) in candidates {
            if e.tick <= min_tick {
                min_part = p;
                min_tick = e.tick;
                min_event = e;
            } else {
                continue;
            }
        }

        match min_part {
            Part::KickDrum => {
                self.kick.next();
                min_group_length = self.kick_length;
            },
            Part::SnareDrum => {
                self.snare.next();
                min_group_length = self.snare_length;
            },
            Part::HiHat => {
                self.hihat.next();
                min_group_length = self.hihat_length;
            },
            Part::CrashCymbal => {
                self.crash.next();
                min_group_length = self.crash_length;
            },
        };

        if min_event.tick < self.limit {
            self.time = self.time + min_event.tick;
            if self.time > min_group_length {
                let remainder = Tick(self.time.0 % min_group_length.0);
                min_event.tick = self.time + remainder;
                Some(min_event)
            } else {
                Some(min_event)
            }
        } else {
            None
        }
    }
}

// Returns time as a number of ticks from beginning, has to be turned into the midi delta-time.
fn flatten_and_merge<'a>(
    mut groups: HashMap<Part, Vec<Group>>,
    time_signature: TimeSignature,
) -> EventIterator<Tick> {
    let f = |p| {
        groups
        .get(&p)
        .map(|g| flatten_groups(p, g))
        .unwrap_or(EventGrid::new())
    };
    let kick = f(Part::KickDrum);
    let snare = f(Part::SnareDrum);
    let hihat = f(Part::HiHat);
    let crash = f(Part::CrashCymbal);
    EventIterator::new(kick, snare, hihat, crash, Tick(1000000))
}

// The length of a beat is not standard, so in order to fully describe the length of a MIDI tick the MetaMessage::Tempo event should be present.
pub fn create_smf<'a>(groups: HashMap<Part, Vec<Group>>, time_signature: TimeSignature) -> Smf<'a> {
    let tracks = vec![] ; // create_tracks(groups, time_signature); // FIXME
                                                        // https://majicdesigns.github.io/MD_MIDIFile/page_timing.html
                                                        // says " If it is not specified the MIDI default is 48 ticks per quarter note."
                                                        // As it's required in `Header`, let's use the same value.
    let metrical = midly::Timing::Metrical(u15::new(TICKS_PER_QUARTER_NOTE));
    Smf {
        header: Header {
            format: midly::Format::Parallel,
            timing: metrical,
        },
        tracks: tracks,
    }
}

// /// Translates drum parts to a single MIDI track.
// fn create_tracks<'a>(
//     parts_and_groups: HashMap<Part, Vec<Group>>,
//     time_signature: TimeSignature, // tempo: u32
// ) -> Vec<Vec<midly::TrackEvent<'a>>> {
//     //FIXME: unhardcode time signature
//     let event_grid = flatten_and_merge(parts_and_groups, TimeSignature { numerator: 4, denominator: BasicLength::Fourth );
//     let mut drums = Vec::new();
//     // let midi_tempo = MidiTempo::from_tempo(Tempo(130)).0;
//     // drums.push(TrackEvent { delta: u28::from(0), kind: TrackEventKind::Meta(MetaMessage::Tempo(midi_tempo)) });
//     // drums.push(TrackEvent { delta: u28::from(0), kind: TrackEventKind::Meta(MetaMessage::TimeSignature(4, 4, MIDI_CLOCKS_PER_CLICK.clone(), 8))});
//     for event in event_grid.events {
//         let midi_message = match event.event_type {
//             EventType::NoteOn(part) => MidiMessage::NoteOn {
//                 key: part.to_midi_key(),
//                 vel: u7::from(120),
//             },
//             EventType::NoteOff(part) => MidiMessage::NoteOff {
//                 key: part.to_midi_key(),
//                 vel: u7::from(0),
//             },
//         };
//         drums.push(TrackEvent {
//             delta: u28::from(event.tick.0 as u32),
//             kind: TrackEventKind::Midi {
//                 channel: u4::from(10),
//                 message: midi_message,
//             },
//         })
//     }

//     vec![drums]
// }
