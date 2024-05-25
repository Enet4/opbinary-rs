//! CLI tool to convert DROv2 files to OPB files.
use opbinary::opb::{Chunk, OplCommand};
use snafu::prelude::*;
use snafu::{report, Whatever};

use opbinary::{Dro, OpbMusic, Uint7Plus};

#[report]
fn main() -> Result<(), Whatever> {
    let args = std::env::args().collect::<Vec<_>>();
    // Expecting 2 arguments: input file and output file
    if args.len() != 3 {
        eprintln!("Usage: dro2opb <input.dro> <output.opb>");
        std::process::exit(-1);
    }
    let input = &args[1];
    let output = &args[2];

    let music = Dro::from_file(input).whatever_context("Failed to read DRO file")?;

    let music = convert_dro_to_opb(music)?;

    if music.chunks.is_empty() {
        eprintln!("No OPL commands found in DRO file");
    }

    music
        .write_to_file(output)
        .whatever_context("Failed to write OPB file")?;
    Ok(())
}

fn convert_dro_to_opb(dro: Dro) -> Result<OpbMusic, Whatever> {
    let mut opb = OpbMusic::new_v1();
    let opbinary::dro::Header::V2(header) = dro.header else {
        whatever!("DRO v1.0 (v0.1) is unsupported");
    };
    let mut current_chunk = Chunk {
        time_elapsed: Uint7Plus::ZERO,
        lo_commands: vec![],
        hi_commands: vec![],
    };
    let mut k = 0;
    for data in dro.data {
        // handle delay commands
        if data.register_index == header.short_delay_code {
            // if current chunk isn't empty, then it is complete
            if !current_chunk.lo_commands.is_empty() || !current_chunk.hi_commands.is_empty() {
                opb.chunks.push(current_chunk);
                // start a new chunk with a delay
                current_chunk = Chunk {
                    time_elapsed: Uint7Plus::from(data.value + 1),
                    lo_commands: vec![],
                    hi_commands: vec![],
                };
            } else {
                // add the delay to the current chunk
                current_chunk.time_elapsed += Uint7Plus::from(data.value + 1);
            }
            continue;
        } else if data.register_index == header.long_delay_code {
            // if current chunk isn't empty, then it is complete
            if !current_chunk.lo_commands.is_empty() || !current_chunk.hi_commands.is_empty() {
                opb.chunks.push(current_chunk);
                // start a new chunk with the delay
                current_chunk = Chunk {
                    time_elapsed: Uint7Plus::from((data.value as u16 + 1) << 8),
                    lo_commands: vec![],
                    hi_commands: vec![],
                };
            } else {
                // add the delay to the current chunk
                current_chunk.time_elapsed += Uint7Plus::from((data.value as u16 + 1) << 8);
            }
            continue;
        }
        k += 1;

        // else, it's an OPL command

        // which OPL chip to write to
        let channel = data.register_index >> 7;

        // the OPL register to write to
        let register = header.codemap[(data.register_index & 0x7F) as usize];

        // !!! fix this

        // handle commands by pushing it to lo or hi
        if channel == 0 {
            // push to lo
            current_chunk.lo_commands.push(
                OplCommand {
                    register,
                    data: data.value,
                }
                .into(),
            );
        } else {
            // push to hi
            current_chunk.hi_commands.push(
                OplCommand {
                    register,
                    data: data.value,
                }
                .into(),
            );
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
