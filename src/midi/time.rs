extern crate derive_more;
use crate::dsl::dsl::{BasicLength, KnownLength, Group, GroupOrNote, Note, FOURTH, Times, EIGHTH};
use std::cmp::Ordering;

use std;

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

impl KnownLength for TimeSignature {
    fn to_128th(&self) -> u32 {
        self.denominator.to_128th() * self.numerator as u32
    }
}

impl TimeSignature {
    fn converges_with(&self, other: TimeSignature) -> Result<(u32, TimeSignature), String> {
        let d: u32 = std::cmp::max(self.denominator, other.denominator)
            .to_note_length()
            .into();
        let d1: u32 = self.denominator.to_note_length().into();
        let d2: u32 = other.denominator.to_note_length().into();
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

    /// The function takes a vector of time signatures and tries to find out if they all converge over a finite number of bars.
    ///
    /// Arguments:
    ///
    /// * `time_signatures`: `time_signatures` is a vector of `TimeSignature` structs. The function
    /// `converges` takes this vector as input and iterates over it using the `iter()` method. It then
    /// uses the `try_fold()` method to fold over the vector and accumulate a result.
    ///
    /// Returns:
    ///
    /// Returns the number of bars to converge + suggested time signature.
    /// Returns Err if signatures won't converge.
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

    /// Returns number of bars in the specified time signature that it takes to converge with the group.
    /// Otherwise returns Err.
    fn converges_over<T: KnownLength>(&self, over: T) -> Result<u32, String> {
        let bar_len = self.to_128th();
        let mut bars = 1;
        let group_len = over.to_128th();
        let mut out = Err("Do not converge".to_string());
        let limit = 1000;

        while bars <= limit {
            let bars_len = bar_len * bars;
            if bars_len % group_len == 0 {
                // return
                out = Ok(bars);
                break
            }

            if bars == limit {
                break;
            } else {
                bars += 1
            }
        }
        out
    }
}

#[test]
fn test_converges_over() {
    let four_fourth = TimeSignature {
        numerator: 4,
        denominator: BasicLength::Fourth,
    };
    let three_fourth_group = Group {
        notes: vec![GroupOrNote::SingleNote(Note::Hit)],
        length: FOURTH.clone(),
        times: Times(3)
    };
    let thirteen_eights = Group {
        notes: vec![GroupOrNote::SingleNote(Note::Hit)],
        length: FOURTH.clone(),
        times: Times(12)
    };
    let in_shards_poly = Group {
        notes: vec![GroupOrNote::SingleNote(Note::Hit), GroupOrNote::SingleNote(Note::Rest), GroupOrNote::SingleGroup(thirteen_eights)],
        length: EIGHTH.clone(),
        times: Times(1)
    };
    assert_eq!(four_fourth.converges_over(three_fourth_group), Ok(3));
    assert_eq!(four_fourth.converges_over(in_shards_poly), Ok(13));
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
    let thirteen_eights = TimeSignature {
        numerator: 13,
        denominator: BasicLength::Eighth,
    };
    assert_eq!(
        three_sixteenth.converges_with(four_fourth),
        Ok((3, four_fourth))
    );
    assert_eq!(
        thirteen_eights.converges_with(four_fourth),
        Ok((15, four_fourth))
    )
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
    let thirteen_eights = TimeSignature {
        numerator: 13,
        denominator: BasicLength::Eighth,
    };
    assert_eq!(
        three_sixteenth.converges(vec![four_fourth, three_fourth, four_fourth]),
        Ok((3, four_fourth))
    );
    assert_eq!(
        four_fourth.converges(vec![thirteen_eights]),
        Ok((15, four_fourth))
    );
}
