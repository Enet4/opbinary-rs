//! Data types used in the protocol.

use core::fmt::Write as _;

use snafu::Snafu;

/// Error parsing an uint7+ value
#[derive(Debug, PartialEq, Snafu)]
pub enum ParseError {
    /// Insufficient bytes to parse uint7+ (byte #{byte})
    InsufficientBytes { byte: u8 },
}

/// uint7+: An unsigned integer type which can be up to 29 bits long.
///
/// In its encoded form,
/// the high bit of each byte indicates whether another byte follows it.
/// As such, the type can contain 7, 14, 21, or 29 bits of data.
///
/// Unlike most other integers in the OPB format,
/// this one is encoded in Little Endian.
#[derive(Debug, Default, Copy, Clone, Eq, Hash, PartialEq, Ord, PartialOrd)]
pub struct Uint7Plus(u32);

impl core::fmt::Display for Uint7Plus {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        core::fmt::Display::fmt(&self.0, f)
    }
}

/// Integer too large for uint7+ (29 bits maximum)
#[derive(Debug, Default, Copy, Clone, Eq, Hash, PartialEq, Snafu)]
pub struct InvalidUint7PlusError;

impl From<u16> for Uint7Plus {
    fn from(value: u16) -> Self {
        Self(value as u32)
    }
}

impl From<u8> for Uint7Plus {
    fn from(value: u8) -> Self {
        Self(value as u32)
    }
}

impl TryFrom<u32> for Uint7Plus {
    type Error = InvalidUint7PlusError;

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        if value > 0x1FFF_FFFF {
            Err(InvalidUint7PlusError)
        } else {
            Ok(Self(value))
        }
    }
}

impl core::ops::AddAssign for Uint7Plus {
    fn add_assign(&mut self, other: Self) {
        self.0 += other.0;
    }
}

impl core::ops::Add<Uint7Plus> for Uint7Plus {
    type Output = Self;
    fn add(self, other: Self) -> Self {
        Uint7Plus(self.0 + other.0)
    }
}

impl Uint7Plus {
    pub const MAX: Self = Self(0x1FFF_FFFF);
    pub const MIN: Self = Self(0);
    pub const ZERO: Self = Self(0);

    pub fn new(value: u32) -> Self {
        TryFrom::try_from(value).expect("Integer too large for uint7+ (29 bits maximum)")
    }

    pub fn to_u32(self) -> u32 {
        self.0
    }

    /// Parse a uint7+ from a byte slice,
    /// returning the parsed value and the remaining bytes
    pub fn parse(from: &[u8]) -> Result<(Self, &[u8]), ParseError> {
        // byte 0
        let byte0 = from
            .first()
            .copied()
            .ok_or(ParseError::InsufficientBytes { byte: 0 })? as u32;

        if byte0 & 0x80 == 0 {
            return Ok((Self(byte0), &from[1..]));
        }
        let byte0 = byte0 & 0x7F;

        // byte 1
        let byte1 = from
            .get(1)
            .copied()
            .ok_or(ParseError::InsufficientBytes { byte: 1 })? as u32;

        if byte1 & 0x80 == 0 {
            return Ok((Self(byte0 | (byte1 << 7)), &from[2..]));
        }
        let byte1 = byte1 & 0x7F;

        // byte 2
        let byte2 = from
            .get(2)
            .copied()
            .ok_or(ParseError::InsufficientBytes { byte: 2 })? as u32;

        if byte2 & 0x80 == 0 {
            return Ok((Self(byte0 | (byte1 << 7) | (byte2 << 14)), &from[3..]));
        }
        let byte2 = byte2 & 0x7F;

        // byte 3
        let byte3 = from
            .get(3)
            .copied()
            .ok_or(ParseError::InsufficientBytes { byte: 3 })? as u32;

        Ok((
            Self(byte0 | (byte1 << 7) | (byte2 << 14) | (byte3 << 21)),
            &from[4..],
        ))
    }

    /// Write the integer to a mutable slice,
    /// returning the number of bytes written.
    ///
    /// If the slice is too short to contain the integer,
    /// the result may be trimmed.
    pub fn write_to_slice(self, to: &mut [u8]) -> u8 {
        let mut value = self.0;
        let mut k = 0;
        for byte in to.iter_mut().take(4) {
            *byte = (value & 0x7F) as u8;
            value >>= 7;
            k += 1;
            if value == 0 {
                break;
            }
            *byte |= 0x80;
        }
        k
    }

    #[cfg(feature = "std")]
    pub fn write_to(self, mut to: impl std::io::Write) -> std::io::Result<u8> {
        let mut buf = [0u8; 4];
        let k = self.write_to_slice(&mut buf);
        to.write_all(&buf[..k as usize])?;
        Ok(k)
    }

    /// Get the number of bytes needed to encode this integer.
    pub fn byte_length(self) -> u8 {
        if self.0 < (1 << 7) {
            1
        } else if self.0 < (1 << 14) {
            2
        } else if self.0 < (1 << 21) {
            3
        } else {
            4
        }
    }
}

/// Display an array of bytes as ASCII text
pub(crate) struct Ascii<T>(pub T);

impl<T> core::fmt::Debug for Ascii<T>
where
    T: AsRef<[u8]>,
{
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.write_char('"')?;
        core::fmt::Display::fmt(self, f)?;
        f.write_char('"')?;
        Ok(())
    }
}

impl<T> core::fmt::Display for Ascii<T>
where
    T: AsRef<[u8]>,
{
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        for &byte in self.0.as_ref() {
            if byte.is_ascii_graphic() {
                write!(f, "{}", byte as char)?;
            } else {
                write!(f, "\\x{:02X}", byte)?;
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::Uint7Plus;

    #[test]
    fn can_parse_uint7plus() {
        let bytes = [0];
        assert_eq!(Uint7Plus::parse(&bytes), Ok((Uint7Plus(0), &[][..])));

        let bytes = [0x7F, 0x80, 0x81, 0x82, 0x83];
        assert_eq!(
            Uint7Plus::parse(&bytes),
            Ok((Uint7Plus(0x7F), &[0x80, 0x81, 0x82, 0x83][..]))
        );

        let bytes = [0xa5, 0x5a, 0x33, 0x44, 0x55];
        assert_eq!(
            Uint7Plus::parse(&bytes),
            Ok((Uint7Plus(0x2d25), &[0x33, 0x44, 0x55][..]))
        );

        let bytes = [0x80, 0x80, 0x7F, 0x55, 0x55];
        assert_eq!(
            Uint7Plus::parse(&bytes),
            Ok((Uint7Plus(0x1f_c000), &[0x55, 0x55][..]))
        );

        let bytes = [0x80, 0x80, 0x80, 0x01];
        assert_eq!(
            Uint7Plus::parse(&bytes),
            Ok((Uint7Plus(0x20_0000), &[][..]))
        );

        let bytes = [0xFF, 0xFF, 0xFF, 0xFF, 0x05];
        assert_eq!(
            Uint7Plus::parse(&bytes),
            Ok((Uint7Plus(0x1FFF_FFFF), &[0x05][..]))
        );

        // error cases
        assert!(Uint7Plus::parse(&[]).is_err());
        assert!(Uint7Plus::parse(&[0xFF]).is_err());
        assert!(Uint7Plus::parse(&[0x80, 0x80, 0x81]).is_err());
    }

    #[test]
    fn can_write_uint7plus() {
        let buf = &mut [0x55; 5];
        assert_eq!(Uint7Plus(0).write_to_slice(buf), 1);
        assert_eq!(buf, &[0x00, 0x55, 0x55, 0x55, 0x55]);

        let buf = &mut [0x55; 5];
        assert_eq!(Uint7Plus(0x7F).write_to_slice(buf), 1);
        assert_eq!(buf, &[0x7f, 0x55, 0x55, 0x55, 0x55]);

        let buf = &mut [0x55; 5];
        assert_eq!(Uint7Plus(0x1f_c000).write_to_slice(buf), 3);
        assert_eq!(buf, &[0x80, 0x80, 0x7F, 0x55, 0x55]);

        let buf = &mut [0x55; 5];
        assert_eq!(Uint7Plus(0x1FFF_FFFF).write_to_slice(buf), 4);
        assert_eq!(buf, &[0xFF, 0xFF, 0xFF, 0xFF, 0x55]);
    }
}
