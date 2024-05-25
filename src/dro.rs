//! Implementation of the DOSBox raw OPL capture format

use snafu::Snafu;

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;
#[cfg(feature = "std")]
use snafu::ResultExt;

#[cfg(feature = "std")]
use std::path::Path;

use crate::data_types::Ascii;

/// An error reading or parsing a DRO file
#[derive(Debug, Snafu)]
#[non_exhaustive]
#[cfg(feature = "std")]
pub enum Error {
    /// Failed to read the file
    ReadFile { source: std::io::Error },
    /// Failed to parse DRO data
    ParseDro { source: ParseError },
}

/// An error reading or parsing a DRO file
#[derive(Debug, Snafu)]
#[non_exhaustive]
#[cfg(not(feature = "std"))]
pub enum Error {
    /// Failed to parse DRO data
    ParseDro { source: ParseError },
}

#[derive(Debug, Snafu)]
#[non_exhaustive]
pub enum ParseError {
    /// Insufficient bytes to parse {context}
    InsufficientBytes { context: &'static str },
    /// Invalid signature
    InvalidSignature,
    /// Unsupported version {version_major}.{version_minor}
    UnsupportedVersion {
        version_major: u16,
        version_minor: u16,
    },
    /// Unsupported format {format}
    UnsupportedFormat { format: u8 },
    /// Song data parsing in version {version_major}.{version_minor} is not supported
    UnsupportedSongData {
        version_major: u16,
        version_minor: u16,
    },
}

/// A complete in-memory representation
/// of a DRO recording file.
#[derive(Debug)]
pub struct Dro {
    /// The first part of the header
    pub file_header: FileHeader,
    /// The second part of the header
    pub header: Header,
    /// The song data.
    ///
    /// Note: this can only hold v2.0 song data commands at the moment
    pub data: Vec<SongData>,
}

impl Dro {
    /// Read a DRO file from a file in disk
    #[cfg(feature = "std")]
    pub fn from_file(path: impl AsRef<Path>) -> Result<Self, Error> {
        let data = std::fs::read(path).context(ReadFileSnafu)?;
        Self::parse(&data).context(ParseDroSnafu)
    }

    /// Take the input slice and parse it as a complete DRO header.
    pub fn parse_header(input: &[u8]) -> Result<(FileHeader, Header, &[u8]), ParseError> {
        let (file_header, input) = FileHeader::parse(input)?;
        let (header, input) = match file_header.version_major {
            0 => {
                let (header, input) = V01Header::parse(input)?;
                (Header::V01(header), input)
            }
            2 => {
                let (header, input) = V2Header::parse(input)?;
                (Header::V2(header), input)
            }
            v => {
                return UnsupportedVersionSnafu {
                    version_major: v,
                    version_minor: file_header.version_minor,
                }
                .fail()
            }
        };

        Ok((file_header, header, input))
    }

    /// Parse the complete DRO file from a byte slice
    pub fn parse(input: &[u8]) -> Result<Self, ParseError> {
        let (file_header, header, input) = Self::parse_header(input)?;

        let Header::V2(v2header) = &header else {
            return UnsupportedSongDataSnafu {
                version_major: file_header.version_major,
                version_minor: file_header.version_minor,
            }
            .fail();
        };

        if v2header.format != V2Header::FORMAT_INTERLEAVED {
            return UnsupportedFormatSnafu {
                format: v2header.format,
            }
            .fail();
        }

        let mut data = Vec::new();
        let mut input = input;

        for _ in 0..v2header.length_pairs {
            let (record, rest) = SongData::parse(input)?;
            data.push(record);
            input = rest;
        }

        Ok(Self {
            file_header,
            header,
            data,
        })
    }
}

/// A DRO file header
#[derive(Clone, PartialEq)]
pub struct FileHeader {
    /// `b"DBRAWOPL"` (not null-terminated)
    pub signature: [u8; 8],
    /// Major version number
    pub version_major: u16,
    /// Minor version number
    pub version_minor: u16,
}

impl FileHeader {
    pub fn parse(input: &[u8]) -> Result<(Self, &[u8]), ParseError> {
        if input.len() < 18 {
            return Err(ParseError::InsufficientBytes {
                context: "FileHeader",
            });
        }

        let (signature, input) = input.split_at(8);
        let signature = signature.try_into().unwrap();
        if &signature != b"DBRAWOPL" {
            return InvalidSignatureSnafu.fail();
        }

        let version_major = u16::from_le_bytes(input[0..2].try_into().unwrap());
        let version_minor = u16::from_le_bytes(input[2..4].try_into().unwrap());

        Ok((
            Self {
                signature,
                version_major,
                version_minor,
            },
            &input[4..],
        ))
    }
}

impl core::fmt::Debug for FileHeader {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("FileHeader")
            .field("signature", &Ascii(self.signature))
            .field("version_major", &self.version_major)
            .field("version_minor", &self.version_minor)
            .finish()
    }
}

/// A DRO header with support for various versions
#[derive(Debug, Clone, PartialEq)]
pub enum Header {
    V01(V01Header),
    V2(V2Header),
}

/// Header that follows the file header
/// in a version 0.1 file
#[derive(Debug, Clone, PartialEq)]
pub struct V01Header {
    /// Length of the song in milliseconds
    pub length_ms: u32,
    /// Length of the song data in bytes
    pub length_bytes: u32,
    /// Flag listing the hardware used in the song
    pub hardware_type: u8,
    /// Rest of the `hardware_type` or song data
    pub hardware_extra: [u8; 3],
}

impl V01Header {
    pub const HARDWARE_TYPE_OPL2: u8 = 0;
    pub const HARDWARE_TYPE_OPL3: u8 = 1;
    pub const HARDWARE_TYPE_DUAL_OPL2: u8 = 2;

    pub fn parse(input: &[u8]) -> Result<(Self, &[u8]), ParseError> {
        if input.len() < 12 {
            return Err(ParseError::InsufficientBytes {
                context: "Header (v0.1)",
            });
        }

        let length_ms = u32::from_le_bytes(input[0..4].try_into().unwrap());
        let length_bytes = u32::from_le_bytes(input[4..8].try_into().unwrap());
        let hardware_type = input[8];
        let hardware_extra = input[9..12].try_into().unwrap();

        Ok((
            Self {
                length_ms,
                length_bytes,
                hardware_type,
                hardware_extra,
            },
            &input[12..],
        ))
    }
}

/// Header that follows the file header
/// in a version 2.0 file
#[derive(Debug, Clone, PartialEq)]
pub struct V2Header {
    /// Length of the song in register/value pairs
    pub length_pairs: u32,
    /// Length of the song data in milliseconds
    pub length_ms: u32,
    /// Flag listing the hardware used in the song
    pub hardware_type: u8,
    /// Data arrangement
    pub format: u8,
    /// Compression type (0 = none, nothing else is supported)
    pub compression: u8,
    /// Command code for short delay (1..256 ms).
    ///
    /// The delay in milliseconds is calculated as
    /// `short_delay_code + 1`.
    pub short_delay_code: u8,
    /// Command code for long delay (> 256 ms).
    ///
    /// The delay in milliseconds is calculated as
    /// `(long_delay_code + 1) * 256)`.
    pub long_delay_code: u8,
    /// Number of entries in codemap table.
    ///
    /// This number is always expected to be 128 or less.
    pub codemap_length: u8,
    /// Codemap table buffer
    /// (values after `codemap_length` are ignored)
    pub codemap: [u8; 128],
}

impl V2Header {
    pub const HARDWARE_TYPE_OPL2: u8 = 0;
    pub const HARDWARE_TYPE_DUAL_OPL2: u8 = 1;
    pub const HARDWARE_TYPE_OPL3: u8 = 2;

    /// Commands and data are interleaved (default)
    pub const FORMAT_INTERLEAVED: u8 = 0;
    pub const FORMAT_ALL_COMMANDS_ALL_DATA: u8 = 1;

    pub fn parse(input: &[u8]) -> Result<(Self, &[u8]), ParseError> {
        if input.len() < 148 {
            return Err(ParseError::InsufficientBytes {
                context: "Header (v2.0)",
            });
        }

        let length_pairs = u32::from_le_bytes(input[0..4].try_into().unwrap());
        let length_ms = u32::from_le_bytes(input[4..8].try_into().unwrap());
        let hardware_type = input[8];
        let format = input[9];
        let compression = input[10];
        let short_delay_code = input[11];
        let long_delay_code = input[12];
        let codemap_length = input[13];

        let full_header_size = 14 + codemap_length as usize;

        if input.len() < full_header_size {
            return Err(ParseError::InsufficientBytes {
                context: "Header (v2.0) codemap",
            });
        }

        let mut codemap = [0; 128];
        codemap[0..codemap_length as usize].copy_from_slice(&input[14..full_header_size]);

        Ok((
            Self {
                length_pairs,
                length_ms,
                hardware_type,
                format,
                compression,
                short_delay_code,
                long_delay_code,
                codemap_length,
                codemap,
            },
            &input[full_header_size..],
        ))
    }
}

/// A single OPL song data record in DRO v2.0
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct SongData {
    /// The index in the codemap table
    /// to the register to write to.
    ///
    /// The high bit identifies which OPL chip to write to.
    pub register_index: u8,
    /// The value to write to the OPL register
    pub value: u8,
}

impl SongData {
    pub fn parse(input: &[u8]) -> Result<(Self, &[u8]), ParseError> {
        if input.len() < 2 {
            return Err(ParseError::InsufficientBytes {
                context: "SongData",
            });
        }

        let register_index = input[0];
        let value = input[1];

        Ok((
            Self {
                register_index,
                value,
            },
            &input[2..],
        ))
    }
}
