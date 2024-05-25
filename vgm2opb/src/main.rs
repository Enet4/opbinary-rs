//! CLI tool to convert VGM files to OPB files.
use opbinary::opb::{self, Chunk};
use opbinary::vgm::{self, OplVgm};
use snafu::prelude::*;
use snafu::{report, Whatever};

use opbinary::{OpbMusic, Uint7Plus, Vgm};

#[report]
fn main() -> Result<(), Whatever> {
    let args = std::env::args().collect::<Vec<_>>();
    // Expecting 2 arguments: input file and output file
    if args.len() != 3 {
        eprintln!("Usage: vgm2opb <input.vgm> <output.opb>");
        std::process::exit(-1);
    }
    let input = &args[1];
    let output = &args[2];

    let music = Vgm::from_file(input).whatever_context("Failed to read VGM file")?;

    let music = music.into_opl_vgm();

    let music = convert_vgm_to_opb(music)?;

    if music.chunks.is_empty() {
        eprintln!("No OPL commands found in DRO file");
    }

    music
        .write_to_file(output)
        .whatever_context("Failed to write OPB file")?;
    Ok(())
}

fn convert_vgm_to_opb(vgm: OplVgm) -> Result<OpbMusic, Whatever> {
    let mut opb = OpbMusic::new_v1();
    let mut current_chunk = Chunk {
        time_elapsed: Uint7Plus::ZERO,
        lo_commands: vec![],
        hi_commands: vec![],
    };

    let mut k = 0;
    for cmd in vgm.opl_commands {
        // uniformize wait commands
        let cmd = if matches!(cmd, vgm::OplCommand::Wait735) {
            vgm::OplCommand::Wait { samples: 735 }
        } else if matches!(cmd, vgm::OplCommand::Wait882) {
            vgm::OplCommand::Wait { samples: 882 }
        } else {
            cmd
        };

        match cmd {
            // handle delay commands
            vgm::OplCommand::Wait { samples } => {
                // convert samples to milliseconds:
                // all VGM sample values are in 44100 Hz
                let ms = (samples as u32 * 1_000 / 44_100) as u16;

                // if current chunk isn't empty, then it is complete
                if !current_chunk.lo_commands.is_empty() || !current_chunk.hi_commands.is_empty() {
                    opb.chunks.push(current_chunk);
                    // start a new chunk with the delay
                    current_chunk = Chunk {
                        time_elapsed: Uint7Plus::from(ms),
                        lo_commands: vec![],
                        hi_commands: vec![],
                    };
                } else {
                    // add the delay to the current chunk
                    current_chunk.time_elapsed += Uint7Plus::from(ms);
                }
            }
            cmd => {
                k += 1;
                // else, it's an OPL command

                let (channel, address, data) = match cmd {
                    vgm::OplCommand::Opl2 { address, data } => (0, address, data),
                    vgm::OplCommand::Opl3 {
                        port,
                        address,
                        data,
                    } => (port, address, data),
                    _ => unreachable!(),
                };

                // handle commands by pushing it to lo or hi
                if channel == 0 {
                    // push to lo
                    current_chunk.lo_commands.push(
                        opb::OplCommand {
                            register: address,
                            data,
                        }
                        .into(),
                    );
                } else {
                    // push to hi
                    current_chunk.hi_commands.push(
                        opb::OplCommand {
                            register: address,
                            data,
                        }
                        .into(),
                    );
                }
            }
        }
    }

    // if current chunk isn't empty, push it to opb
    if !current_chunk.lo_commands.is_empty() || !current_chunk.hi_commands.is_empty() {
        opb.chunks.push(current_chunk);
    }

    let chunk_count = opb.chunks.len() as u32;
    opb.header.chunk_count = chunk_count;

    println!("Wrote {} commands into {} chunks", k, chunk_count);

    Ok(opb)
}
