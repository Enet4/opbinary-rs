//! Baseline implementation for VGM file reading and writing
//!
//! Support for this format may be limited.

use core::fmt;
use snafu::{ensure, OptionExt as _, Snafu};
#[cfg(feature = "std")]
use std::path::Path;

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use snafu::ResultExt;

use crate::data_types::Ascii;

#[cfg(not(feature = "std"))]
macro_rules! eprintln {
    ($_: expr, ) => {
        /* no-op */
    };
    ($_: expr, $_2: expr) => {
        /* no-op */
    };
    ($_: expr, $_2: expr, $_3: expr) => {
        /* no-op */
    };
}

/// An error reading a VGM file
#[cfg(feature = "std")]
#[derive(Debug, Snafu)]
#[non_exhaustive]
pub enum Error {
    /// Failed to read the file
    ReadFile { source: std::io::Error },
    /// Failed to write the file
    WriteFile { source: std::io::Error },
    /// Not a valid VGM file
    InvalidVgm,
    /// Failed to parse VGM data
    ParseVgm { source: ParseError },
    /// Failed to parse VGM command #{index}
    ParseCommandVgm { index: u32, source: ParseError },
    /// Unsupported VGM version {version:04x}
    UnsupportedVersion { version: u32 },
}

/// An error reading a VGM file
#[cfg(not(feature = "std"))]
#[derive(Debug, Snafu)]
#[non_exhaustive]
pub enum Error {
    /// Not a valid VGM file
    InvalidVgm,
    /// Failed to parse VGM data
    ParseVgm { source: ParseError },
    /// Failed to parse VGM command #{index}
    ParseCommandVgm { index: u32, source: ParseError },
    /// Unsupported VGM version {version:04x}
    UnsupportedVersion { version: u32 },
}

pub type Result<T, E = Error> = core::result::Result<T, E>;

/// Error parsing a VGM file or VGM file component
#[derive(Debug, PartialEq, Snafu)]
pub enum ParseError {
    /// Insufficient bytes to parse {context}
    InsufficientBytes { context: &'static str },
    /// Insufficient bytes to parse command {code:02x} ({operand_size} operand bytes)
    InsufficientBytesCommand { code: u8, operand_size: u8 },
    /// Unknown command {code:#04x}
    UnknownCommand { code: u8 },
}

/// A full VGM file object,
/// with limited metadata.
///
/// All commands are retained (including non-OPL commands),
/// but many pieces of information from the header will not be read
/// and versions >=1.60 are not supported.
#[derive(Debug)]
pub struct Vgm {
    /// a partial header
    pub header: OplPartialHeader,
    /// a sequence of VGM commands
    pub commands: Vec<Command>,
}

impl Vgm {
    #[cfg(feature = "std")]
    pub fn from_file(path: impl AsRef<Path>) -> Result<Self> {
        let data = std::fs::read(path).context(ReadFileSnafu)?;
        Self::from_bytes(&data)
    }

    pub fn from_bytes(input: &[u8]) -> Result<Self> {
        let (header, rest) = OplPartialHeader::parse(input).context(ParseVgmSnafu)?;

        // check identifier
        ensure!(header.base_header.ident == *b"Vgm ", InvalidVgmSnafu);

        // check version
        if header.base_header.version > 0x00000151 {
            return UnsupportedVersionSnafu {
                version: header.base_header.version,
            }
            .fail();
        }

        let mut command_data = Vec::new();

        // read until EOF
        let mut rest = &rest[..header.base_header.eof_offset as usize - 210];
        let mut i: u32 = 0;

        while !rest.is_empty() {
            let (command, new_rest) =
                Command::parse(rest).context(ParseCommandVgmSnafu { index: i })?;
            let code = command.code;
            command_data.push(command);
            if code == 0x66 {
                break;
            }
            rest = new_rest;
            i += 1;
        }

        Ok(Self {
            header,
            commands: command_data,
        })
    }

    /// Interpret the VGM commands and return an OPL-specific VGM music object.
    pub fn into_opl_vgm(self) -> OplVgm {
        let opl_commands = self
            .commands
            .into_iter()
            .filter_map(|command| match command.code {
                // Wait
                0x61 => {
                    let samples = u16::from_le_bytes(command.operands[..2].try_into().unwrap());
                    Some(OplCommand::Wait { samples })
                }
                0x62 => Some(OplCommand::Wait735),
                0x63 => Some(OplCommand::Wait882),
                0x70..=0x7f => Some(OplCommand::SmallWait {
                    n: command.code - 0x70,
                }),
                // YM3812 commands
                0x5A => {
                    let address = command.operands[0];
                    let data = command.operands[1];
                    Some(OplCommand::Opl2 { address, data })
                }
                // YMF262 commands, port 0 or 1
                0x5e | 0x5f => {
                    let address = command.operands[0];
                    let data = command.operands[1];
                    Some(OplCommand::Opl3 {
                        port: command.code - 0x5e,
                        address,
                        data,
                    })
                }
                // End of sound data
                0x66 => None,
                _ => {
                    eprintln!("Unhandled command of code {:#04x}", command.code);
                    None
                }
            })
            .collect();
        OplVgm {
            header: self.header,
            opl_commands,
        }
    }

    #[cfg(feature = "std")]
    pub fn write_to_file(&self, file: impl AsRef<Path>) -> Result<()> {
        use std::io::BufWriter;
        let writer = BufWriter::new(std::fs::File::create(file).context(WriteFileSnafu)?);
        self.write_to(writer)
    }

    #[cfg(feature = "std")]
    pub fn write_to_vec(&self, out: &mut Vec<u8>) -> Result<()> {
        self.write_to(out)?;
        Ok(())
    }

    #[cfg(feature = "std")]
    pub fn write_to(&self, writer: impl std::io::Write) -> Result<()> {
        let mut writer = writer;

        // encode header first
        let encoded_header = self.encoded_header();

        let header_size = match self.header.base_header.version {
            0..=0x0151 => 128,
            _ => 256,
        };

        writer
            .write_all(&encoded_header[..header_size])
            .context(WriteFileSnafu)?;

        // encode OPL commands
        for command in &self.commands {
            writer.write_all(&[command.code]).context(WriteFileSnafu)?;
            writer
                .write_all(&command.operands[..command.operand_size as usize])
                .context(WriteFileSnafu)?;
        }

        Ok(())
    }

    /// Note: 256 bytes are allocated,
    /// But we may end up printing less than that
    /// depending on the VGM version.
    #[cfg(feature = "std")]
    fn encoded_header(&self) -> [u8; 256] {
        // ignore total_samples and calculate it instead
        let total_samples = self.calculate_total_samples();

        let mut header = [0; 256];
        header[..4].copy_from_slice(b"Vgm ");
        header[4..8].copy_from_slice(&self.header.base_header.eof_offset.to_le_bytes());
        header[8..12].copy_from_slice(&self.header.base_header.version.to_le_bytes());
        header[0x14..0x18].copy_from_slice(&self.header.gd3_offset.to_le_bytes());
        header[0x18..0x1c].copy_from_slice(&total_samples.to_le_bytes());
        header[0x1c..0x20].copy_from_slice(&self.header.loop_offset.to_le_bytes());
        header[0x20..0x24].copy_from_slice(&self.header.loop_samples.to_le_bytes());
        header[0x50..0x54].copy_from_slice(&self.header.ym3812_clock.to_le_bytes());
        header[0x5c..0x60].copy_from_slice(&self.header.ymf262_clock.to_le_bytes());
        header
    }

    #[cfg(feature = "std")]
    fn calculate_total_samples(&self) -> u32 {
        self.commands
            .iter()
            .take_while(|command| command.code != 0x66)
            .filter_map(|command| match command.code {
                0x61 => Some(u16::from_le_bytes(command.operands[..2].try_into().unwrap()) as u32),
                0x62 => Some(735),
                0x63 => Some(882),
                _ => None,
            })
            .sum()
    }
}

/// A VGM file object,
/// with support limited to OPL chips and OPL commands.
#[derive(Debug)]
pub struct OplVgm {
    pub header: OplPartialHeader,
    pub opl_commands: Vec<OplCommand>,
}

/// Base header of all VGM files
#[derive(Clone, PartialEq)]
pub struct BaseHeader {
    /// "Vgm " ident
    ///
    /// file identification (0x56 0x67 0x6d 0x20)
    pub ident: [u8; 4],
    /// Eof offset
    ///
    /// Relative offset to end of file (i.e. file length - 4).
    /// This is mainly used to find the next track
    /// when concatenating player stubs and multiple files.
    pub eof_offset: u32,
    /// Version number
    ///
    /// Version number in BCD-Code.
    /// e.g. Version 1.71 is stored as 0x00000171.
    /// This is used for backwards compatibility in players,
    /// and defines which header values are valid.
    pub version: u32,
}

impl core::fmt::Debug for BaseHeader {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("BaseHeader")
            .field("ident", &Ascii(&self.ident))
            .field("eof_offset", &self.eof_offset)
            .field("version", &self.version)
            .finish()
    }
}

impl BaseHeader {
    pub fn parse(input: &[u8]) -> Result<(Self, &[u8]), ParseError> {
        if input.len() < 12 {
            return Err(ParseError::InsufficientBytes {
                context: "Base header",
            });
        }
        let ident = [input[0], input[1], input[2], input[3]];
        let eof_offset = u32::from_le_bytes([input[4], input[5], input[6], input[7]]);
        let version = u32::from_le_bytes([input[8], input[9], input[10], input[11]]);
        Ok((
            Self {
                ident,
                eof_offset,
                version,
            },
            &input[12..],
        ))
    }
}

/// A partial view of the VGM header
/// specifically for keeping data relevant to OPL music.
#[derive(Debug, Clone, PartialEq)]
pub struct OplPartialHeader {
    pub base_header: BaseHeader,

    /// Relative offset to GD3 tag.
    ///
    /// 0 if no GD3 tag.
    /// GD3 tags are descriptive tags similar in use to ID3 tags in MP3 files.
    /// See the GD3 specification for more details.
    /// The GD3 tag is usually stored at the end of the file,
    /// immediately after the VGM data.
    ///
    /// Address: `0x14`
    pub gd3_offset: u32,

    /// Total of all wait values in the file.
    ///
    /// Address: `0x18`
    pub total_samples: u32,

    /// Relative offset to loop point, or 0 if no loop.
    ///
    /// For example, if the data for the one-off intro to a song was
    /// in bytes `0x0040 - 0x3FFF` of the file,
    /// but the main looping section started at `0x4000`,
    /// this would contain the value `0x4000 - 0x1C = 0x00003FE4`.
    pub loop_offset: u32,

    /// Number of samples in one loop, or 0 if there is no loop.
    ///
    /// Total of all wait values between the loop point and the end of the file.
    ///
    /// Address: `0x20`
    pub loop_samples: u32,

    /// Input clock rate in Hz for the YM3812 chip. A typical value is 3579545.
    ///
    /// It should be 0 if there is no YM3812 chip used.
    ///
    /// Address: `0x50`
    pub ym3812_clock: u32,

    /// Input clock rate in Hz for the YMF262 chip. A typical value is 14318180.
    ///
    /// It should be 0 if there is no YMF262 chip used.
    ///
    /// Address: `0x5C`
    pub ymf262_clock: u32,
}

impl OplPartialHeader {
    pub fn parse(input: &[u8]) -> Result<(Self, &[u8]), ParseError> {
        ensure!(
            input.len() >= 256,
            InsufficientBytesSnafu {
                context: "VGM header"
            }
        );

        let (base_header, _) = BaseHeader::parse(input)?;

        let gd3_offset = u32::from_le_bytes(input[0x14..0x18].try_into().unwrap());
        let total_samples = u32::from_le_bytes(input[0x18..0x1c].try_into().unwrap());
        let loop_offset = u32::from_le_bytes(input[0x1c..0x20].try_into().unwrap());
        let loop_samples = u32::from_le_bytes(input[0x20..0x24].try_into().unwrap());
        let ym3812_clock = u32::from_le_bytes(input[0x50..0x54].try_into().unwrap());
        let ymf262_clock = u32::from_le_bytes(input[0x5c..0x60].try_into().unwrap());

        let header_size = match base_header.version {
            0..=0x0151 => 128,
            _ => 256,
        };

        Ok((
            Self {
                base_header,
                gd3_offset,
                total_samples,
                loop_offset,
                loop_samples,
                ym3812_clock,
                ymf262_clock,
            },
            &input[header_size..],
        ))
    }
}

/// A command in a VGM file
/// which is supported in the context of reading and writing OPL music.
#[derive(Clone, PartialEq)]
pub enum OplCommand {
    /// Command of code `0x5A` (OPL2)
    Opl2 {
        /// The register to write to
        address: u8,
        /// The data to write to the register
        data: u8,
    },
    /// Command of code `0x5E` or `0x5F` (OPL3),
    Opl3 {
        /// Port number (0 or 1)
        port: u8,
        /// The register to write to
        address: u8,
        /// The data to write to the register
        data: u8,
    },
    /// Command of code `0x61` (Wait n samples)
    Wait {
        /// The number of samples to wait
        samples: u16,
    },
    /// Command of code `0x62`,
    /// to wait 735 samples.
    Wait735,
    /// Command of code `0x63`,
    /// to wait 882 samples.
    Wait882,
    /// Command of code `0x7n` (Wait n+1 samples)
    SmallWait {
        /// The number of samples to wait minus 1
        /// (n must range from 0 to 15)
        n: u8,
    },
}

impl fmt::Debug for OplCommand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            OplCommand::Opl2 { address, data } => write!(
                f,
                "OPL2 {{ address: {:#04x}, data: {:#04x} }}",
                address, data
            ),
            OplCommand::Opl3 {
                port,
                address,
                data,
            } => write!(
                f,
                "OPL3 {{ port: {:#04x}, address: {:#04x}, data: {:#04x} }}",
                port, address, data
            ),
            OplCommand::Wait { samples } => write!(f, "Wait {} samples", samples),
            OplCommand::Wait735 => write!(f, "Wait 735 samples"),
            OplCommand::Wait882 => write!(f, "Wait 882 samples"),
            OplCommand::SmallWait { n: samples } => write!(f, "Wait {} samples", samples + 1),
        }
    }
}

/// An arbitrary command in a VGM file,
/// uninterpreted
#[derive(Clone, PartialEq)]
pub struct Command {
    /// the code
    pub code: u8,
    /// the total length of the operands in bytes
    pub operand_size: u8,
    /// the operand bytes in order (unused operands are left as 0)
    pub operands: [u8; 8],
}

impl Command {
    pub fn parse(input: &[u8]) -> Result<(Self, &[u8]), ParseError> {
        let (&code, rest) = input.split_first().context(InsufficientBytesSnafu {
            context: "Command code",
        })?;

        let operand_size = Command::operand_size_of(code).context(UnknownCommandSnafu { code })?;

        ensure!(
            rest.len() >= operand_size as usize,
            InsufficientBytesCommandSnafu { code, operand_size }
        );

        let mut operands = [0; 8];
        operands[..(operand_size as usize)].copy_from_slice(&rest[..(operand_size as usize)]);
        Ok((
            Self {
                code,
                operand_size,
                operands,
            },
            &input[1 + operand_size as usize..],
        ))
    }

    /// the number of bytes expected after the command code
    /// (as per version 1.51)
    fn operand_size_of(code: u8) -> Option<u8> {
        Some(match code {
            // 0 byte operands
            0x62 | 0x63 | 0x66 | 0x70..=0x7f | 0x80..=0x8f => 0,
            // 1 byte operands
            0x30..=0x3F | 0x4f | 0x50 => 1,
            // 1 byte operands in v1.51, 2 bytes v1.60 onwards
            0x41..=0x4E => 1,
            // 2 byte operands
            0x40 | 0x50..=0x5f | 0x61 | 0xa0 | 0xb0..=0xbf => 2,
            // 3 byte operands
            0xc0..=0xc8 | 0xd0 | 0xd1 | 0xd2 | 0xd3 | 0xd4 | 0xd5 | 0xd6 => 3,
            // 4 byte operands
            0xe0 | 0xe1 => 4,
            // not supported
            _ => return None,
        })
    }
}

impl fmt::Debug for Command {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Command {{ code: {:#04x}, operands: {:02x?} }}",
            self.code,
            &self.operands[..(self.operand_size as usize)]
        )
    }
}

impl From<OplCommand> for Command {
    fn from(value: OplCommand) -> Self {
        match value {
            OplCommand::Wait735 => Command {
                code: 0x62,
                operand_size: 0,
                operands: [0; 8],
            },
            OplCommand::Wait882 => Command {
                code: 0x63,
                operand_size: 0,
                operands: [0; 8],
            },
            OplCommand::SmallWait { n: samples } => Command {
                code: 0x70 + samples,
                operand_size: 0,
                operands: [0; 8],
            },
            OplCommand::Wait { samples } => Command {
                code: 0x61,
                operand_size: 2,
                operands: [samples as u8, (samples >> 8) as u8, 0, 0, 0, 0, 0, 0],
            },
            OplCommand::Opl2 { address, data } => Command {
                code: 0x5A,
                operand_size: 2,
                operands: [address, data, 0, 0, 0, 0, 0, 0],
            },
            OplCommand::Opl3 {
                port,
                address,
                data,
            } => Command {
                code: 0x5E + port,
                operand_size: 2,
                operands: [address, data, 0, 0, 0, 0, 0, 0],
            },
        }
    }
}
