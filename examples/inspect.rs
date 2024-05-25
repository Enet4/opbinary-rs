//! Inspect an OPB file
//!
use opbinary::{vgm::Vgm, Dro, OpbMusic};

use snafu::{prelude::*, report, Whatever};

#[report]
fn main() -> Result<(), Whatever> {
    let file = std::env::args().nth(1).expect("missing file argument");

    if file.ends_with(".dro") {
        let music = Dro::from_file(&file).whatever_context("Failed to read DRO file")?;
        println!("{:#?}", music);
    } else if file.ends_with(".opb") {
        let music = OpbMusic::from_file(&file).whatever_context("Failed to read OPB file")?;
        println!("{:#?}", music);
    } else if file.ends_with(".vgm") {
        let music = Vgm::from_file(&file).whatever_context("Failed to read VGM file")?;
        // convert to OPL VGM
        let music = music.into_opl_vgm();
        println!("{:#?}", music);
    } else {
        panic!("unknown file type");
    }

    Ok(())
}
