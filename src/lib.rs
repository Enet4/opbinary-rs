//! Implementation of OPL constructs.
//!
//! This crate provides a high-level interface for working with OPL data.
//! A quick overview of the modules:
//!
//! - `dro`: [DOSBox Raw OPL (DRO) v2][dro] file format support
//! - `opb`: [OPL binary (OPB) v1][opb] file format support
//! - `vgm`: [Video Game Music][vgm] file format support (OPL music only)
//! - `opl`: raw OPL command definitions
//! - `registers`: helper OPL register definitions
//!
//! [dro]: https://moddingwiki.shikadi.net/wiki/DRO_Format
//! [opb]: https://github.com/Enichan/OPBinaryLib
//! [vgm]: https://vgmrips.net/wiki/VGM_Specification
#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(not(feature = "std"))]
extern crate alloc;

pub(crate) mod data_types;
pub mod dro;
pub mod opb;
pub mod opl;
pub mod registers;
pub mod vgm;

pub use crate::data_types::Uint7Plus;
pub use crate::dro::{Dro, Error as DroError};
pub use crate::opb::{Error as OpbError, OpbMusic};
pub use crate::vgm::{Error as VgmError, Vgm};
