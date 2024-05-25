//! Implementation of the OPB file specification.

use snafu::Snafu;
#[cfg(feature = "std")]
use std::path::Path;

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

#[cfg(feature = "std")]
use crate::registers::is_valid_opb_register;

use crate::data_types::{Ascii, Uint7Plus};
use snafu::ResultExt;

/// An error reading an OPB file
#[cfg(feature = "std")]
#[derive(Debug, Snafu)]
#[non_exhaustive]
pub enum Error {
    /// Failed to read the file
    ReadFile { source: std::io::Error },
    /// Failed to write the file
    WriteFile { source: std::io::Error },
    /// Failed to write OPB data
    WriteOpb { source: std::io::Error },
    /// Failed to parse OPB data
    ParseOpb { source: ParseError },
    /// Invalid OPB File
    InvalidOpb,
    /// Unexpected OPB file size: expected {expected} bytes, got {actual} bytes
    TruncatedOpb { expected: usize, actual: usize },
    /// Unsupported version {version}
    UnsupportedVersion { version: u8 },
    /// Unsupported format {format} (only standard format is supported)
    UnsupportedFormat { format: u8 },
    /// Command count too large to re-encode to OPB
    CommandCountTooLarge {
        source: crate::data_types::InvalidUint7PlusError,
    },
}

/// An error reading an OPB file
#[cfg(not(feature = "std"))]
#[derive(Debug, Snafu)]
#[non_exhaustive]
pub enum Error {
    /// Failed to parse OPB data
    ParseOpb { source: ParseError },
    /// Invalid OPB File
    InvalidOpb,
    /// Unexpected OPB file size: expected {expected} bytes, got {actual} bytes
    TruncatedOpb { expected: usize, actual: usize },
    /// Unsupported version {version}
    UnsupportedVersion { version: u8 },
    /// Unsupported format {format} (only standard format is supported)
    UnsupportedFormat { format: u8 },
    /// Command count too large to re-encode to OPB
    CommandCountTooLarge {
        source: crate::data_types::InvalidUint7PlusError,
    },
}

pub type Result<T, E = Error> = core::result::Result<T, E>;

/// A complete in-memory representation of an OPL music in the OPB format.
#[derive(Debug)]
pub struct OpbMusic {
    pub file_id: FileId,
    pub header: Header,
    pub instruments: Vec<Instrument>,
    pub chunks: Vec<Chunk>,
}

impl OpbMusic {
    /// Create a new empty OPB v1 file
    pub fn new_v1() -> Self {
        OpbMusic {
            file_id: FileId {
                code: *b"OPBin",
                version: b'1',
                zero: 0,
            },
            header: Header {
                format: 0,
                size: 0,
                instrument_count: 0,
                chunk_count: 0,
            },
            instruments: Vec::new(),
            chunks: Vec::new(),
        }
    }

    #[cfg(feature = "std")]
    pub fn from_file(file: impl AsRef<Path>) -> Result<Self> {
        let bytes = std::fs::read(file).context(ReadFileSnafu)?;
        Self::parse(&bytes)
    }

    #[cfg(feature = "std")]
    pub fn write_to_file(&self, file: impl AsRef<Path>) -> Result<()> {
        use std::io::BufWriter;
        let writer = BufWriter::new(std::fs::File::create(file).context(WriteFileSnafu)?);
        self.write_to(writer)
    }

    #[cfg(feature = "std")]
    pub fn write_to(&self, writer: impl std::io::Write) -> Result<()> {
        let mut writer = writer;

        // encode complete header first
        let encoded_header = self.encoded_header();
        writer.write_all(&encoded_header).context(WriteFileSnafu)?;

        // write each instrument
        for instrument in &self.instruments {
            let encoded_instrument = instrument.encoded();
            writer
                .write_all(&encoded_instrument)
                .context(WriteFileSnafu)?;
        }

        // write each chunk
        for chunk in &self.chunks {
            chunk.write_to(&mut writer)?;
        }
        Ok(())
    }

    #[cfg(feature = "std")]
    pub fn write_to_vec(&self, out: &mut Vec<u8>) -> Result<()> {
        self.write_to(out)?;
        Ok(())
    }

    pub fn parse(bytes: &[u8]) -> Result<Self> {
        let input_len = bytes.len();
        let (file_id, bytes) = FileId::parse(bytes).context(ParseOpbSnafu)?;

        // check file ID portion to ensure it's an OPB file
        if file_id.code != *b"OPBin" {
            return InvalidOpbSnafu.fail();
        }

        // don't try any further if the version is not '1'
        if file_id.version != b'1' {
            return UnsupportedVersionSnafu {
                version: file_id.version,
            }
            .fail();
        }

        let (header, bytes) = Header::parse(bytes).context(ParseOpbSnafu)?;

        // check whether we have enough bytes to read the entire file
        if bytes.len() + (header.size as usize) < input_len {
            return TruncatedOpbSnafu {
                expected: header.size as usize,
                actual: input_len,
            }
            .fail();
        }

        // check whether it's in standard mode or raw mode

        if header.format != 0 {
            return UnsupportedFormatSnafu {
                format: header.format,
            }
            .fail();
        }

        let mut bytes = bytes;
        let mut instruments = Vec::new();
        for _ in 0..header.instrument_count {
            let (instrument, rest) = Instrument::parse(bytes).context(ParseOpbSnafu)?;
            instruments.push(instrument);
            bytes = rest;
        }
        let mut chunks = Vec::new();
        for _ in 0..header.chunk_count {
            let (chunk, rest) = Chunk::parse(bytes).context(ParseOpbSnafu)?;
            chunks.push(chunk);
            bytes = rest;
        }

        Ok(Self {
            file_id,
            header,
            instruments,
            chunks,
        })
    }

    fn encoded_header(&self) -> [u8; 20] {
        // do not trust size_bytes,
        // calculate expected size instead
        let size_bytes = self.calculate_size_bytes().to_be_bytes();
        let instrument_count_bytes = self.header.instrument_count.to_be_bytes();
        let chunk_count_bytes = self.header.chunk_count.to_be_bytes();
        [
            self.file_id.code[0],
            self.file_id.code[1],
            self.file_id.code[2],
            self.file_id.code[3],
            self.file_id.code[4],
            self.file_id.version,
            self.file_id.zero,
            self.header.format,
            size_bytes[0],
            size_bytes[1],
            size_bytes[2],
            size_bytes[3],
            instrument_count_bytes[0],
            instrument_count_bytes[1],
            instrument_count_bytes[2],
            instrument_count_bytes[3],
            chunk_count_bytes[0],
            chunk_count_bytes[1],
            chunk_count_bytes[2],
            chunk_count_bytes[3],
        ]
    }

    fn calculate_size_bytes(&self) -> u32 {
        // 20 bytes for the header
        // 9 bytes for each instrument
        let mut count = 20 + 9 * self.header.instrument_count;

        // accumulate chunk sizes
        for chunk in &self.chunks {
            count += chunk.calculate_size_bytes();
        }
        count
    }
}

/// Error parsing an OPB file or OPB file component
#[derive(Debug, PartialEq, Snafu)]
pub enum ParseError {
    /// Insufficient bytes to parse {context}
    InsufficientBytes { context: &'static str },
    /// Insufficient bytes to parse {context} #{index}
    InsufficientBytesAtIndex { context: &'static str, index: u32 },
}

#[derive(Clone, PartialEq)]
pub struct FileId {
    /// "OPBin"
    pub code: [u8; 5],
    /// Version (starting at the character '1', not 0x1)
    pub version: u8,
    /// Must be 0x0
    pub zero: u8,
}

impl core::fmt::Debug for FileId {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("FileId")
            .field("code", &Ascii(self.code))
            .field("version", &self.version)
            .field("zero", &self.zero)
            .finish()
    }
}

impl FileId {
    pub fn parse(bytes: &[u8]) -> Result<(Self, &[u8]), ParseError> {
        if bytes.len() < 7 {
            return Err(ParseError::InsufficientBytes { context: "FileId" });
        }

        let (bytes, rest) = bytes.split_at(7);

        let field = FileId {
            code: bytes[0..5].try_into().unwrap(),
            version: bytes[5],
            zero: bytes[6],
        };

        Ok((field, rest))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Header {
    /// Format (0 = standard, 1 = raw)
    pub format: u8,
    /// Size in bytes
    pub size: u32,
    /// InstrumentCount
    pub instrument_count: u32,
    /// ChunkCount
    pub chunk_count: u32,
}

impl Header {
    pub fn parse(bytes: &[u8]) -> Result<(Self, &[u8]), ParseError> {
        if bytes.len() < 13 {
            return Err(ParseError::InsufficientBytes { context: "Header" });
        }

        let (bytes, rest) = bytes.split_at(13);

        let field = Header {
            format: bytes[0],
            size: u32::from_be_bytes(bytes[1..5].try_into().unwrap()),
            instrument_count: u32::from_be_bytes(bytes[5..9].try_into().unwrap()),
            chunk_count: u32::from_be_bytes(bytes[9..13].try_into().unwrap()),
        };

        Ok((field, rest))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Instrument {
    /// Feedback/connection (base reg C0)
    pub feedback: u8,
    /// Modulator
    pub modulator: Modulator,
    /// Carrier
    pub carrier: Carrier,
}
impl Instrument {
    pub(crate) fn parse(bytes: &[u8]) -> Result<(Self, &[u8]), ParseError> {
        if bytes.len() < 9 {
            return Err(ParseError::InsufficientBytes {
                context: "Instrument",
            });
        }
        let (bytes, rest) = bytes.split_at(9);

        let out = Instrument {
            feedback: bytes[0],
            modulator: Modulator {
                characteristic: bytes[1],
                attack_decay_level: bytes[2],
                sustain_release_level: bytes[3],
                wave_select: bytes[4],
            },
            carrier: Carrier {
                characteristic: bytes[5],
                attack_decay_level: bytes[6],
                sustain_release_level: bytes[7],
                wave_select: bytes[8],
            },
        };

        Ok((out, rest))
    }

    fn encoded(&self) -> [u8; 9] {
        [
            self.feedback,
            self.modulator.characteristic,
            self.modulator.attack_decay_level,
            self.modulator.sustain_release_level,
            self.modulator.wave_select,
            self.carrier.characteristic,
            self.carrier.attack_decay_level,
            self.carrier.sustain_release_level,
            self.carrier.wave_select,
        ]
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Modulator {
    /// Characteristic (base reg 20) (Mult, KSR, EG, VIB and AM flags)
    pub characteristic: u8,
    /// Attack/decay level (base reg 60)
    pub attack_decay_level: u8,
    /// Sustain/release level (base reg 80)
    pub sustain_release_level: u8,
    /// Wave select (base reg E0)
    pub wave_select: u8,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Carrier {
    // [uint8] Characteristic (base reg 23) (Mult, KSR, EG, VIB and AM flags)
    pub characteristic: u8,
    // [uint8] Attack/decay level (base reg 63)
    pub attack_decay_level: u8,
    // [uint8] Sustain/release level (base reg 83)
    pub sustain_release_level: u8,
    // [uint8] Wave select (base reg E3)
    pub wave_select: u8,
}

/// The header of a chunk
/// (without the Lo and Hi OPL commands)
#[derive(Debug, Clone, PartialEq)]
pub struct ChunkHeader {
    /// Time elapsed since last chunk (in milliseconds)
    pub time_elapsed: Uint7Plus,
    /// OPL_CommandCountLo
    pub opl_command_count_lo: Uint7Plus,
    /// OPL_CommandCountHi
    pub opl_command_count_hi: Uint7Plus,
}

/// A partial OPL command representation
/// which uses 8 bits to represent an OPL register.
///
/// Whether the 8 bits in `register`
/// refer to the channels in`0x0xx` or to the channels in `0x1xx`
/// is defined by the presence in the _Lo_ or _Hi_ command list.
#[derive(Clone, PartialEq)]
pub struct OplCommand {
    /// Register
    pub register: u8,
    /// The data to write to the register
    pub data: u8,
}

/// Debug implementation overridden to
/// always write in hexadecimal and in a single line
impl core::fmt::Debug for OplCommand {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(
            f,
            "OplCommand {{ register: {:02X}, data: 0x{:02X} }}",
            self.register, self.data
        )
    }
}

/// An extended set of the OPL command
/// which takes advantage of unused registers in the range `0xD0..0xDF`
/// to specify additional commands.
#[derive(Clone, PartialEq)]
pub enum OpbCommand {
    /// A standard OPL command
    Opl(OplCommand),
    /// Set instrument (0xD0)
    ///
    /// Sets instrument properties for the specified channel
    /// according to the bits in arguments `channel_mask` and `mask`.
    /// For each bit set in such a way,
    /// write the instrument's corresponding properties
    /// to the appropriate offset for the channel and operator
    /// (modulator = operator 1, carried = operator 2).
    SetInstrument {
        /// Instrument index
        instr_index: Uint7Plus,
        /// Channel mask
        channel_mask: u8,
        /// Mask
        mask: u8,
        /// Data byte describing modulator levels data
        /// (OPL register 0x40)
        /// if bit 5 of `channel_mask` is set.
        /// Should be ignored otherwise.
        mod_levels: u8,
        /// Data byte describing carrier levels data
        /// (OPL register 0x40)
        /// if bit 6 of `channel_mask` is set.
        /// Should be ignored otherwise.
        car_levels: u8,
    },
    /// Play Instrument (0xD1)
    PlayInstrument {
        /// Instrument index
        instr_index: Uint7Plus,
        /// Channel mask
        channel_mask: u8,
        /// Mask
        mask: u8,
        /// Frequency data to be written to OPL register 0xA0
        freq: u8,
        /// Note on/off data to be written to OPL register 0xB0
        note: u8,
        /// Data byte describing modulator levels data
        /// (OPL register 0x40)
        /// if bit 5 of `channel_mask` is set.
        /// Should be ignored otherwise.
        mod_levels: u8,
        /// Data byte describing carrier levels data
        /// (OPL register 0x40)
        /// if bit 6 of `channel_mask` is set.
        /// Should be ignored otherwise.
        car_levels: u8,
    },
    /// Combined Note (0xD7 to 0xDF)
    ///
    /// This command combines the data for frequency (OPL register 0xA0)
    /// and note on (OPL register 0xB0).
    /// The top two bits of the note data,
    /// which are unused in the OPL spec,
    /// encode the presence of optional arguments
    /// for modulator volume and carrier volume,
    /// much like the [`SetInstrument`](OpbCommand::SetInstrument) command.
    CombinedNote {
        /// The specific register in the range `0xD7..0xDF`
        register: u8,
        /// Frequency data to be written to OPL register 0xA0
        freq: u8,
        /// Note on/off data to be written to OPL register 0xB0
        note: u8,
        /// Data byte describing modulator levels data
        /// (OPL register 0x40)
        /// if bit 6 of `note` is set.
        /// Should be ignored otherwise.
        mod_levels: u8,
        /// Data byte describing carrier levels data
        /// (OPL register 0x40)
        /// if bit 7 of `note` is set.
        /// Should be ignored otherwise.
        car_levels: u8,
    },
}

impl From<OplCommand> for OpbCommand {
    fn from(command: OplCommand) -> Self {
        OpbCommand::Opl(command)
    }
}

impl core::fmt::Debug for OpbCommand {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            OpbCommand::Opl(command) => write!(f, "{:?}", command),
            OpbCommand::SetInstrument {
                instr_index,
                channel_mask,
                mask,
                mod_levels,
                car_levels,
            } => write!(
                f,
                "SetInstrument {{ register: D0, instr_index: {}, channel_mask: 0x{:02X}, mask: 0x{:02X}, mod_levels: 0x{:02X}, car_levels: 0x{:02X} }}",
                instr_index, channel_mask, mask, mod_levels, car_levels
            ),
            OpbCommand::PlayInstrument {
                instr_index,
                channel_mask,
                mask,
                freq,
                note,
                mod_levels,
                car_levels,
            } => write!(
                f,
                "PlayInstrument {{ register: D1, instr_index: {}, channel_mask: 0x{:02X}, mask: 0x{:02X}, freq: 0x{:02X}, note: 0x{:02X}, mod_levels: 0x{:02X}, car_levels: 0x{:02X} }}",
                instr_index, channel_mask, mask, freq, note, mod_levels, car_levels
            ),
            OpbCommand::CombinedNote {
                register,
                freq,
                note,
                mod_levels,
                car_levels,
            } => write!(
                f,
                "CombinedNote {{ register: {:02X}, freq: 0x{:02X}, note: 0x{:02X}, mod_levels: 0x{:02X}, car_levels: 0x{:02X} }}",
                register, freq, note, mod_levels, car_levels
            ),
        }
    }
}

impl OpbCommand {
    pub(crate) fn parse(bytes: &[u8]) -> Result<(Self, &[u8]), ParseError> {
        if bytes.len() < 2 {
            return Err(ParseError::InsufficientBytes {
                context: "OpbCommand",
            });
        }

        let (command, rest) = match bytes[0] {
            0xD0 => {
                let (instr_index, rest) =
                    Uint7Plus::parse(&bytes[1..]).map_err(|_| ParseError::InsufficientBytes {
                        context: "OpbCommand.SetInstrument.instr_index",
                    })?;
                let channel_mask = rest[0];
                let mask = rest[1];

                // read mod_levels only if bit 5 of channel mask is set
                let read_mod_levels = channel_mask & 0b00100000 != 0;
                let read_car_levels = channel_mask & 0b01000000 != 0;
                let opt_param_count = read_mod_levels as usize + read_car_levels as usize;

                // check if we have enough bytes to read the whole command
                if rest.len() < 2 + opt_param_count {
                    return Err(ParseError::InsufficientBytes {
                        context: "OpbCommand.SetInstrument",
                    });
                }

                let mod_levels = if read_mod_levels { rest[2] } else { 0 };

                // read car_levels only if bit 6 of channel mask is set
                let car_levels = if read_car_levels {
                    rest[2 + read_mod_levels as usize]
                } else {
                    0
                };

                (
                    OpbCommand::SetInstrument {
                        instr_index,
                        channel_mask,
                        mask,
                        mod_levels,
                        car_levels,
                    },
                    &rest[2 + opt_param_count..],
                )
            }
            0xD1 => {
                let (instr_index, rest) =
                    Uint7Plus::parse(&bytes[1..]).map_err(|_| ParseError::InsufficientBytes {
                        context: "OpbCommand.PlayInstrument.instr_index",
                    })?;
                let channel_mask = rest[0];
                let mask = rest[1];
                let freq = rest[2];
                let note = rest[3];
                // read mod_levels only if bit 5 of channel mask is set
                let read_mod_levels = channel_mask & 0b00100000 != 0;
                // read car_levels only if bit 6 of channel mask is set
                let read_car_levels = channel_mask & 0b01000000 != 0;
                let opt_param_count = read_mod_levels as usize + read_car_levels as usize;

                // check if we have enough bytes to read the whole command
                if rest.len() < 4 + opt_param_count {
                    return Err(ParseError::InsufficientBytes {
                        context: "OpbCommand.PlayInstrument",
                    });
                }

                let mod_levels = if read_mod_levels { rest[4] } else { 0 };
                let car_levels = if read_car_levels {
                    rest[4 + read_mod_levels as usize]
                } else {
                    0
                };

                (
                    OpbCommand::PlayInstrument {
                        instr_index,
                        channel_mask,
                        mask,
                        freq,
                        note,
                        mod_levels,
                        car_levels,
                    },
                    &rest[4 + opt_param_count..],
                )
            }
            0xD7..=0xDF => {
                let register = bytes[0];
                let freq = bytes[1];
                let note = bytes[2];

                // read mod_levels if bit 6 of note is set
                let read_mod_levels = note & 0b01000000 != 0;
                // read car_levels if bit 7 of note is set
                let read_car_levels = note & 0b10000000 != 0;
                let opt_param_count = read_mod_levels as usize + read_car_levels as usize;

                let mod_levels = if read_mod_levels { bytes[3] } else { 0 };

                let car_levels = if read_car_levels {
                    bytes[2 + read_mod_levels as usize]
                } else {
                    0
                };

                (
                    OpbCommand::CombinedNote {
                        register,
                        freq,
                        note,
                        mod_levels,
                        car_levels,
                    },
                    &bytes[3 + opt_param_count..],
                )
            }
            _ => {
                let register = bytes[0];
                let data = bytes[1];
                (OpbCommand::Opl(OplCommand { register, data }), &bytes[2..])
            }
        };

        Ok((command, rest))
    }

    /// The OPB register for this command
    pub fn register(&self) -> u8 {
        match self {
            OpbCommand::Opl(command) => command.register,
            OpbCommand::SetInstrument { .. } => 0xD0,
            OpbCommand::PlayInstrument { .. } => 0xD1,
            OpbCommand::CombinedNote { register, .. } => *register,
        }
    }

    #[cfg(feature = "std")]
    pub fn write_to(&self, mut writer: impl std::io::Write) -> Result<()> {
        // depends on the command
        match self {
            OpbCommand::Opl(command) => {
                writer
                    .write_all(&[command.register, command.data])
                    .context(WriteOpbSnafu)?;
            }
            OpbCommand::SetInstrument {
                instr_index,
                channel_mask,
                mask,
                mod_levels,
                car_levels,
            } => {
                writer.write_all(&[0xD0]).context(WriteOpbSnafu)?;
                instr_index.write_to(&mut writer).context(WriteOpbSnafu)?;
                writer
                    .write_all(&[*channel_mask, *mask])
                    .context(WriteOpbSnafu)?;

                // only write mod_levels if bit 5 of channel mask is set
                if channel_mask & 0b00100000 != 0 {
                    writer.write_all(&[*mod_levels]).context(WriteOpbSnafu)?;
                }
                // only write car_levels if bit 6 of channel mask is set
                if channel_mask & 0b01000000 != 0 {
                    writer.write_all(&[*car_levels]).context(WriteOpbSnafu)?;
                }
            }
            OpbCommand::PlayInstrument {
                instr_index,
                channel_mask,
                mask,
                freq,
                note,
                mod_levels,
                car_levels,
            } => {
                writer.write_all(&[0xD1]).context(WriteOpbSnafu)?;
                instr_index.write_to(&mut writer).context(WriteOpbSnafu)?;
                writer
                    .write_all(&[*channel_mask, *mask, *freq, *note])
                    .context(WriteOpbSnafu)?;

                // only write mod_levels if bit 5 of channel mask is set
                if channel_mask & 0b00100000 != 0 {
                    writer.write_all(&[*mod_levels]).context(WriteOpbSnafu)?;
                }
                // only write car_levels if bit 6 of channel mask is set
                if channel_mask & 0b01000000 != 0 {
                    writer.write_all(&[*car_levels]).context(WriteOpbSnafu)?;
                }
            }
            OpbCommand::CombinedNote {
                register,
                freq,
                note,
                mod_levels,
                car_levels,
            } => {
                writer
                    .write_all(&[*register, *freq, *note])
                    .context(WriteOpbSnafu)?;

                // only write mod_levels if bit 6 of note is set
                if note & 0b01000000 != 0 {
                    writer.write_all(&[*mod_levels]).context(WriteOpbSnafu)?;
                }
                // only write car_levels if bit 7 of note is set
                if note & 0b10000000 != 0 {
                    writer.write_all(&[*car_levels]).context(WriteOpbSnafu)?;
                }
            }
        }

        Ok(())
    }

    fn calculate_size_bytes(&self) -> u32 {
        match self {
            OpbCommand::Opl(_) => 2,
            OpbCommand::SetInstrument { channel_mask, .. } => {
                let mut count = 5;
                if channel_mask & 0b00100000 != 0 {
                    count += 1;
                }
                if channel_mask & 0b01000000 != 0 {
                    count += 1;
                }
                count
            }
            OpbCommand::PlayInstrument { channel_mask, .. } => {
                let mut count = 7;
                if channel_mask & 0b00100000 != 0 {
                    count += 1;
                }
                if channel_mask & 0b01000000 != 0 {
                    count += 1;
                }
                count
            }
            OpbCommand::CombinedNote { note, .. } => {
                let mut count = 3;
                if note & 0b01000000 != 0 {
                    count += 1;
                }
                if note & 0b10000000 != 0 {
                    count += 1;
                }
                count
            }
        }
    }
}

#[derive(Debug)]
pub struct Chunk {
    /// Time elapsed since last chunk (in milliseconds)
    pub time_elapsed: Uint7Plus,
    /// Commands Lo
    pub lo_commands: Vec<OpbCommand>,
    /// Commands Hi
    pub hi_commands: Vec<OpbCommand>,
}

impl Chunk {
    pub(crate) fn parse(bytes: &[u8]) -> Result<(Self, &[u8]), ParseError> {
        let (time_elapsed, bytes) =
            Uint7Plus::parse(bytes).map_err(|_| ParseError::InsufficientBytes {
                context: "Chunk.time_elapsed",
            })?;

        let (command_count_lo, bytes) =
            Uint7Plus::parse(bytes).map_err(|_| ParseError::InsufficientBytes {
                context: "Chunk.command_count_lo",
            })?;

        let (command_count_hi, bytes) =
            Uint7Plus::parse(bytes).map_err(|_| ParseError::InsufficientBytes {
                context: "Chunk.command_count_hi",
            })?;

        let mut lo_commands = Vec::new();
        let mut hi_commands = Vec::new();
        let mut bytes = bytes;

        for i in 0..command_count_lo.to_u32() {
            let (command, rest) =
                OpbCommand::parse(bytes).map_err(|_| ParseError::InsufficientBytesAtIndex {
                    context: "Chunk.lo_command",
                    index: i,
                })?;
            #[cfg(feature = "std")]
            if !is_valid_opb_register(command.register()) {
                eprintln!("Invalid command #{i}: {command:?}");
            }
            lo_commands.push(command);
            bytes = rest;
        }

        for i in 0..command_count_hi.to_u32() {
            let (command, rest) =
                OpbCommand::parse(bytes).map_err(|_| ParseError::InsufficientBytesAtIndex {
                    context: "Chunk.hi_command",
                    index: i,
                })?;
            #[cfg(feature = "std")]
            if !is_valid_opb_register(command.register()) {
                eprintln!("Invalid command #{i}: {command:?}");
            }
            hi_commands.push(command);
            bytes = rest;
        }

        Ok((
            Self {
                time_elapsed,
                lo_commands,
                hi_commands,
            },
            bytes,
        ))
    }

    #[cfg(feature = "std")]
    fn write_to(&self, mut writer: impl std::io::Write) -> Result<()> {
        // write time elapsed
        self.time_elapsed
            .write_to(&mut writer)
            .context(WriteOpbSnafu)?;

        // write command count lo
        Uint7Plus::try_from(self.lo_commands.len() as u32)
            .context(CommandCountTooLargeSnafu)?
            .write_to(&mut writer)
            .context(WriteOpbSnafu)?;

        // write command count hi
        Uint7Plus::try_from(self.hi_commands.len() as u32)
            .context(CommandCountTooLargeSnafu)?
            .write_to(&mut writer)
            .context(WriteOpbSnafu)?;

        // write lo commands
        for command in &self.lo_commands {
            command.write_to(&mut writer)?;
        }

        // write hi commands
        for command in &self.hi_commands {
            command.write_to(&mut writer)?;
        }

        Ok(())
    }

    fn calculate_size_bytes(&self) -> u32 {
        // up to 4 bytes for time_elapsed, command_count_lo, command_count_hi
        let count = self.time_elapsed.byte_length()
            + Uint7Plus::try_from(self.hi_commands.len() as u32)
                .unwrap_or(Uint7Plus::MAX)
                .byte_length()
            + Uint7Plus::try_from(self.lo_commands.len() as u32)
                .unwrap_or(Uint7Plus::MAX)
                .byte_length();
        let mut count = u32::from(count);

        // count each OPB command
        for command in &self.lo_commands {
            count += command.calculate_size_bytes();
        }
        count
    }
}
