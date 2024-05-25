//! Helper module for OPL register addresses

use core::ops::Range;

/// Test LSI / Enable waveform control
pub const TEST_LSI: u8 = 0x01;

/// Timer 1 data
pub const TIMER1_DATA: u8 = 0x02;

/// Timer 2 data
pub const TIMER2_DATA: u8 = 0x03;

/// Timer control flags
pub const TIMER_CONTROL: u8 = 0x04;

/// Speech synthesis mode / Keyboard split note select
pub const SPEECH_SYNTHESIS: u8 = 0x08;

/// Amp Mod / Vibrato / EG type / Key Scaling / Multiple
pub const AMP_MOD_VIBRATO_EG_TYPE_KEY_SCALING_MULTIPLE_RANGE: Range<u8> = 0x20..0x35;

/// Key scaling level / Operator output level
pub const KEY_SCALING_LEVEL_OPERATOR_OUTPUT_LEVEL_RANGE: Range<u8> = 0x40..0x55;

/// Attack Rate / Decay Rate
pub const ATTACK_DECAY_RATE_RANGE: Range<u8> = 0x60..0x75;

/// Sustain Level / Release Rate
pub const SUSTAIN_RELEASE_RATE_RANGE: Range<u8> = 0x80..0x95;

/// Frequency (low 8 bits)
pub const FREQUENCY_LOW_RANGE: Range<u8> = 0xA0..0xA8;

/// Key On / Octave / Frequency (high 2 bits)
pub const KEY_ON_OCTAVE_FREQUENCY_HIGH_RANGE: Range<u8> = 0xB0..0xB8;

///  AM depth / Vibrato depth / Rhythm control
pub const AM_VIBRATO_RHYTHM: u8 = 0xBD;

/// Feedback strength / Connection type
pub const FEEDBACK_CONNECTION_RANGE: Range<u8> = 0xC0..0xC8;

/// Wave Select
pub const WAVE_SELECT_RANGE: Range<u8> = 0xE0..0xF5;

/// Check whether the given register address is valid
/// for the OPB format
pub fn is_valid_opb_register(register: u8) -> bool {
    match register {
        0x01 | 0x02 | 0x03 | 0x04 | 0x08 | 0xBD => true,
        r if (20..=35).contains(&r) => true,
        r if (0x40..=0x55).contains(&r) => true,
        r if (0x60..=0x75).contains(&r) => true,
        r if (0x80..=0x95).contains(&r) => true,
        r if (0xA0..=0xA8).contains(&r) => true,
        r if (0xB0..=0xB8).contains(&r) => true,
        r if (0xC0..=0xC8).contains(&r) => true,
        r if (0xE0..=0xF5).contains(&r) => true,

        // OPB-specific are considered valid
        0xD0 | 0xD1 | 0xD7..=0xDF => true,
        _ => false,
    }
}

/// Check whether the given OPL register address is valid
pub fn is_valid_opl_register(register: u8) -> bool {
    match register {
        0x01 | 0x02 | 0x03 | 0x04 | 0x08 | 0xBD => true,
        r if (20..=35).contains(&r) => true,
        r if (0x40..=0x55).contains(&r) => true,
        r if (0x60..=0x75).contains(&r) => true,
        r if (0x80..=0x95).contains(&r) => true,
        r if (0xA0..=0xA8).contains(&r) => true,
        r if (0xB0..=0xB8).contains(&r) => true,
        r if (0xC0..=0xC8).contains(&r) => true,
        r if (0xE0..=0xF5).contains(&r) => true,
        _ => false,
    }
}
