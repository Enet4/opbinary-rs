//! CLI tool to convert DROv2 files to VGM files.
use opbinary::vgm::{self, OplPartialHeader};
use snafu::prelude::*;
use snafu::{report, Whatever};

use opbinary::{Dro, Vgm};

#[report]
fn main() -> Result<(), Whatever> {
    let args = std::env::args().collect::<Vec<_>>();
    // Expecting 2 arguments: input file and output file
    if args.len() != 3 {
        eprintln!("Usage: dro2vgm <input.dro> <output.vgm>");
        std::process::exit(-1);
    }
    let input = &args[1];
    let output = &args[2];

    let music = Dro::from_file(input).whatever_context("Failed to read DRO file")?;

    if music.data.is_empty() {
        eprintln!("No OPL commands found in DRO file");
    }

    let music = convert_dro_to_vgm(music)?;

    music
        .write_to_file(output)
        .whatever_context("Failed to write VGM file")?;
    Ok(())
}

fn convert_dro_to_vgm(dro: Dro) -> Result<Vgm, Whatever> {
    let opbinary::dro::Header::V2(header) = dro.header else {
        whatever!("DRO v1.0 (v0.1) is unsupported");
    };

    let mut vgm = Vgm {
        header: OplPartialHeader {
            base_header: vgm::BaseHeader {
                ident: *b"Vgm ",
                eof_offset: 0,
                version: 0x151,
            },
            gd3_offset: 0,
            total_samples: 0,
            loop_offset: 0,
            loop_samples: 0,
            ym3812_clock: 0,
            ymf262_clock: 14318180,
        },
        commands: vec![],
    };
    let mut eof_offset: u32 = 0;
    let mut total_samples = 0;
    for data in &dro.data {
        // handle delay commands
        let delay = if data.register_index == header.short_delay_code {
            data.value as u32 + 1
        } else if data.register_index == header.long_delay_code {
            (data.value as u32 + 1) * 256
        } else {
            0
        };

        if delay > 0 {
            // convert time to samples to wait (44100 samples per second)
            let mut samples = delay * 44_100 / 1_000;

            // push waiting commands until we elapsed enough time
            while samples > 0 {
                // optimize for special cases
                if samples >= 882 {
                    vgm.commands.push(vgm::OplCommand::Wait882.into());
                    samples -= 882;
                    eof_offset += 1;
                } else if samples >= 735 {
                    vgm.commands.push(vgm::OplCommand::Wait735.into());
                    samples -= 735;
                    eof_offset += 1;
                } else {
                    let wait = std::cmp::min(samples, 0xFFFF) as u16;
                    vgm.commands
                        .push(vgm::OplCommand::Wait { samples: wait }.into());
                    samples -= wait as u32;
                    // requires 3 bytes
                    eof_offset += 3;
                }
            }

            total_samples += samples;
            continue;
        }
        // else, it's an OPL command

        // which OPL chip to write to
        let channel = data.register_index >> 7;

        // the OPL register to write to
        let register = header.codemap[(data.register_index & 0x7F) as usize];

        // convert command to VGM OPL3 command
        let cmd: vgm::Command = vgm::OplCommand::Opl3 {
            port: channel,
            address: register,
            data: data.value,
        }
        .into();
        eof_offset += cmd.operand_size as u32 + 1;
        vgm.commands.push(cmd);
    }

    // push end of sound data command
    vgm.commands.push(vgm::Command {
        code: 0x66,
        operand_size: 0,
        operands: [0; 8],
    });
    eof_offset += 1;

    vgm.header.base_header.eof_offset = eof_offset;
    vgm.header.total_samples = total_samples;

    Ok(vgm)
}
