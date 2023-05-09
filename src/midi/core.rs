extern crate derive_more;
use std::collections::HashMap;
use std::ops::Add;

use midly::{live::LiveEvent, num::u15, Header, MidiMessage, Smf, Track};

use crate::dsl::dsl::{
    group_or_delimited_group, groups, BasicLength, Length, Group, ModdedLength, Note, Times,
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
    Tempo(u8),
    Signature(TimeSignature),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct TimeSignature {
    pub numerator: u8,
    pub denominator: BasicLength,
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub enum Part {
    KickDrum,
    SnareDrum,
    HiHat,
    CrashCymbal,
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

impl<T: Add<Tick, Output = T> + Clone> Add for EventGrid<T> {
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
        self.length = self.length + other.length;
        self
    }
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
            delta_grid.events.push(Event { tick: Delta(delta.0), event_type: e.event_type })
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
            Length::Tied(first, second) => {
                first.to_ticks() + second.to_ticks()
            }
            Length::Triplet(mlen) => {
                let Tick(straight) = mlen.to_ticks();
                let triplet = straight * 2 / 3;
                Tick(triplet)
            }
        }
    }
}

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

fn flatten_groups(part: Part, groups: Vec<Group>) -> EventGrid<Tick> {
    let mut time: Tick = Tick(0);
    let mut grid: EventGrid<Tick> = EventGrid::new();
    groups.iter().for_each(|group| {
        grid = grid.clone() + flatten_group(group, part, &mut time);
    });
    grid
}

// Combines multiple sorted EventGrid<Tick>
fn combine_event_grids<'a, T>(a: EventGrid<T>, b : EventGrid<T>) -> EventGrid<T>
where
    T: Ord,
    EventGrid<T>: Add<EventGrid<T>, Output = EventGrid<T>>
{
    let mut all_events = a + b;
    all_events.events.sort_by(|e1, e2| { e1.tick.cmp(&e2.tick)} );
    all_events
}

// Combines a vector of sorted EventGrid<Tick>
fn merge_event_grids<T>(mut eg: Vec<EventGrid<T>>) -> EventGrid<T>
where
    T: Ord,
    EventGrid<T>: Add<EventGrid<T>, Output = EventGrid<T>> + Clone
{
    let first = eg.pop().unwrap();
    eg.iter().fold(first, |mut acc, next| {
        acc = combine_event_grids(acc, (*next).clone());
        acc
    })
}

// Returns time as a number of ticks from beginning, has to be turned into the midi delta-time.
fn flatten_and_merge(groups: HashMap<Part, Vec<Group>>) -> EventGrid<Tick> {
    let mut eg = Vec::new();
    for (part, group) in groups {
        eg.push(flatten_groups(part, group))
    }
    merge_event_grids(eg)
}

// The length of a beat is not standard, so in order to fully describe the length of a MIDI tick the MetaMessage::Tempo event should be present.
pub fn create_smf<'a>(groups: HashMap<Part, Vec<Group>>) -> Smf<'a> {
    let tracks = vec![]; // create_tracks(groups); // FIXME
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

/// Translates drum parts to a single MIDI track.
fn create_tracks<'a>(parts_and_groups: HashMap<Part, Vec<Group>>) -> Vec<Vec<midly::TrackEvent<'a>>> {
    let event_grid = flatten_and_merge(parts_and_groups).to_delta();
}
