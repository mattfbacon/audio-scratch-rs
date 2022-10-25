use once_cell::unsync::Lazy;
use std::io::{ErrorKind, Write};
use std::process::{Command, Stdio};
use std::sync::atomic::{AtomicBool, Ordering};

static RUNNING: AtomicBool = AtomicBool::new(true);
const RATE: f32 = 8000.0; // samples per second
const VOLUME: f32 = 0.2;

fn play(mut player: impl Write, notes: impl Iterator<Item = Note>, beats_per_minute: f32) {
    let beats_per_second = beats_per_minute / 60.0;
    let beats_per_sample = beats_per_second / RATE;

    let mut freq_tick = 0.0;
    for note in notes {
        let mut remaining = note.duration.beats();
        while remaining >= 0.0 {
            if !RUNNING.load(Ordering::Acquire) {
                return;
            }

            let unadjusted = (freq_tick * std::f32::consts::TAU).sin();
            let scaling = u32::MAX as f32 * VOLUME;
            let val_float = (unadjusted + 1.0) * (scaling / 2.0);
            let val = val_float.round() as u32;
            match player.write_all(&val.to_le_bytes()) {
                Err(err) if err.kind() == ErrorKind::BrokenPipe => return,
                other => other.expect("writing to aplay stdin"),
            }

            freq_tick += note.value.freq() / RATE;
            if freq_tick >= note.value.freq() {
                freq_tick = 0.0;
            }

            remaining -= beats_per_sample;
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct NoteDuration(pub f32);

impl core::ops::Add<NoteDuration> for NoteDuration {
    type Output = NoteDuration;

    fn add(self, other: Self) -> Self {
        Self(self.0 + other.0)
    }
}

impl core::ops::Mul<f32> for NoteDuration {
    type Output = NoteDuration;

    fn mul(self, factor: f32) -> Self {
        Self(self.0 * factor)
    }
}

impl NoteDuration {
    pub const WHOLE: Self = Self(4.0);
    pub const HALF: Self = Self(2.0);
    pub const QUARTER: Self = Self(1.0);
    pub const EIGHTH: Self = Self(0.5);
    pub const SIXTEENTH: Self = Self(0.25);

    pub const fn beats(self) -> f32 {
        self.0
    }

    pub fn from_beats(num_beats: f32) -> Self {
        Self::QUARTER * num_beats
    }

    pub fn dotted(self) -> Self {
        self * 1.5
    }
}

#[derive(Debug, Clone, Copy)]
pub struct NoteValue(f32);

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum NoteName {
    C,
    Cs,
    D,
    Ds,
    E,
    F,
    Fs,
    G,
    Gs,
    A,
    As,
    B,
}

impl NoteName {
    pub const fn to_midi_in_octave(self, octave: i32) -> i32 {
        self as u8 as i32 + (12 * octave)
    }
}

impl NoteValue {
    const TWELFTH_ROOT_OF_TWO: Lazy<f32> = Lazy::new(|| 2f32.powf(1.0 / 12.0));
    pub const A440: Self = Self::from_freq(440.0);
    pub const C0: Self = Self::from_freq(16.35160);

    pub const fn freq(self) -> f32 {
        self.0
    }

    pub const fn from_freq(freq: f32) -> Self {
        Self(freq)
    }

    pub fn from_note_in_octave(note: NoteName, octave: i32) -> Self {
        Self::from_midi(note.to_midi_in_octave(octave))
    }

    pub fn from_midi(midi_note: i32) -> Self {
        // using A440 as the center to increase precision
        let relative_to_a440 = midi_note - NoteName::A.to_midi_in_octave(4);
        Self::A440.offset_midi(relative_to_a440)
    }

    pub fn offset_midi(self, midi_offset: i32) -> Self {
        Self(self.freq() * Self::TWELFTH_ROOT_OF_TWO.powi(midi_offset))
    }

    pub fn offset_octave(self, offset: i32) -> Self {
        Self::from_freq(self.freq() * 2f32.powi(offset))
    }
}

#[derive(Debug, Clone, Copy)]
struct Note {
    pub value: NoteValue,
    pub duration: NoteDuration,
}

macro_rules! song {
    [$($value1:tt $value2:tt - $duration:tt),+ $(,)?] => {
        [$(song!(@single ($value1 $value2) - ($duration))),+]
    };

    (@single ($($value:tt)+) - ($($duration:tt)+)) => {
        Note {
            value: song!(@value $($value)+),
            duration: song!(@duration $($duration)+),
        }
    };

    (@value $note_name:ident $octave:tt) => {
        NoteValue::from_note_in_octave(NoteName::$note_name, $octave)
    };

    (@duration whole) => { NoteDuration::WHOLE };
    (@duration half) => { NoteDuration::HALF };
    (@duration quarter) => { NoteDuration::QUARTER };
    (@duration eighth) => { NoteDuration::EIGHTH };
    (@duration sixteenth) => { NoteDuration::SIXTEENTH };
    (@duration dotted $($inner:tt)+) => { song!(@duration $($inner)+).dotted() };
}

fn main() {
    let rand_seed = std::env::var("RAND_SEED").ok().map(|var| var.parse::<u64>().expect("invalid RAND_SEED")) .unwrap_or_else(|| {
        let ret = rand::Rng::gen(&mut rand::thread_rng());
    eprintln!("Your random seed is {ret}. To replay with this seed, pass it as the RAND_SEED environment variable.");
        ret
        });

    let mut player_process = Command::new("aplay")
        .arg("-fU32_LE")
        .arg("-c1")
        .arg(format!("-r{RATE}"))
        .stdin(Stdio::piped())
        .spawn()
        .expect("spawning aplay process");
    let player = player_process
        .stdin
        .as_mut()
        .expect("could not open aplay stdin");

    ctrlc::set_handler(|| {
        RUNNING.store(false, Ordering::Release);
    })
    .expect("setting interrupt handler");

    let mut rng = <rand::rngs::StdRng as rand::SeedableRng>::seed_from_u64(rand_seed);
    play(
        player,
        std::iter::repeat_with(move || rand::Rng::gen_range(&mut rng, 30..70))
            .map(NoteValue::from_midi)
            .map(|value| Note {
                value,
                duration: NoteDuration::SIXTEENTH,
            }),
        120.0,
    );

    player_process.wait().expect("waiting for aplay to finish");
}
