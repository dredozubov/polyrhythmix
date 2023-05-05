extern crate derive_more;
use derive_more::{Mul, Add};
use midly::{Smf, Header, live::LiveEvent, MidiMessage, num::u15};
use std::convert::{TryFrom};

use crate::dsl::dsl::{Group, Length, ModdedLength, BasicLength};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FlatNote {
    // measured in ticks (128th notes), so it's easy to align to a midi grid
    length: u8
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Add, Mul)]
#[repr(transparent)]
pub struct Tick(pub u128);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum EventType {
    NoteOn,
    NoteOff,
    Tempo,
    Signature
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Event {
    tick: Tick,
    event_type: EventType
}

// Events are supposed to be sorted.
pub type EventGrid = Vec<Event>;

static TICKS_PER_QUARTER_NOTE : u16 = 48;

fn basic_length_to_ticks(basic_length: BasicLength) -> Tick {
    match basic_length {
        BasicLength::Whole => Tick((TICKS_PER_QUARTER_NOTE * 4) as u128),
        BasicLength::Half => Tick((TICKS_PER_QUARTER_NOTE * 2) as u128),
        BasicLength::Fourth => Tick(TICKS_PER_QUARTER_NOTE as u128),
        BasicLength::Eighth => Tick((TICKS_PER_QUARTER_NOTE / 2) as u128),
        BasicLength::Sixteenth => Tick((TICKS_PER_QUARTER_NOTE / 4) as u128),
        BasicLength::ThirtySecond => Tick((TICKS_PER_QUARTER_NOTE / 8) as u128),
        BasicLength::SixtyFourth => Tick((TICKS_PER_QUARTER_NOTE / 16) as u128),
    }
}

fn modded_length_to_ticks(modded_length: ModdedLength) -> Tick {
    match modded_length {
        ModdedLength::Plain(blen) => basic_length_to_ticks(blen),
        ModdedLength::Dotted(blen) => {
            let Tick(whole) = basic_length_to_ticks(blen);
            let half = whole / 2;
            Tick(whole + half)
        }
    }
}

fn length_to_ticks(length: Length) -> Tick {
    match length {
        Length::Simple(mlen) => modded_length_to_ticks(mlen),
        Length::Tied(first, second) => modded_length_to_ticks(first) + modded_length_to_ticks(second),
        Length::Triplet(mlen) => {
            let Tick(straight) = modded_length_to_ticks(mlen);
            let triplet = straight * 2 / 3;
            Tick(triplet)
        }
    }
}

fn flatten_group(Group { notes, length, times }: Group, start: Tick) -> EventGrid {
    let mut time = start;
    let note_length = length_to_ticks(length);
    let mut grid = Vec::new();
    let ticks = length_to_ticks(length);
    for entry in notes.iter() { |entry|
        match entry {
            crate::dsl::dsl::GroupOrNote::SingleGroup(_) => todo!(),
            crate::dsl::dsl::GroupOrNote::SingleNote(Rest) => { time = time + note_length },
            crate::dsl::dsl::GroupOrNote::SingleNote(Hit) => {
                let note_end = time + note_length;
                let note_on = Event { tick: time, event_type: EventType::NoteOn };
                let note_off = Event { tick: note_end, event_type: EventType::NoteOff };
                grid.push(note_on);
                grid.push(note_off);
                time = note_end;
            },
        };
    }
    grid.repeat(times.0 as usize)
}

fn flatten_groups(groups: Vec<Group>) -> EventGrid {
    let mut out : EventGrid = Vec::new();
    groups.iter().flat_map(|Group { notes, length, times }| {

    }).collect()
}


// The length of a beat is not standard, so in order to fully describe the length of a MIDI tick the MetaMessage::Tempo event should be present.
fn create_smf<'a>() -> Smf<'a> {
    let tracks = vec![]; // FIXME
    // https://majicdesigns.github.io/MD_MIDIFile/page_timing.html
    // says " If it is not specified the MIDI default is 48 ticks per quarter note."
    // As it's required in `Header`, let's use the same value.
    let metrical = midly::Timing::Metrical(u15::new(TICKS_PER_QUARTER_NOTE.clone()));
    Smf { header: Header { format: midly::Format::Parallel, timing: metrical }, tracks: tracks }
}