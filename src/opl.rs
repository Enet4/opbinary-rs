//! OPL command module
//! 

/// A data structure representing a raw OPL3 command.
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct RawOplCommand {
    /// the time elapsed in milisseconds
    pub timestamp: u16,
    /// the OPL register to write to
    pub register: u16,
    /// the data to write to the register
    pub data: u8,
}
