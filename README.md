# opb-rs

Implementation of OPL constructs and OPL-related file formats in Rust.

This is a work in progress.

## Building

Use the latest stable toolchain of Rust.

```sh
cargo build
# or
cargo build --release
```

## See also

- [`dro2opb`](dro2opb) converts DRO v2 files into OPB.
  `cd drop2opb && cargo run --release «FILE»`
- [`vgm2opb`](vgm2opb) converts VGM files containing OPL music into OPB.
  `cd vgm2opb && cargo run --release «FILE»`
- Run the example `inspect` to read a file and dump its contents:
  `cargo run --example inspect «FILE»`

## License and attribution notice

Licensed under either of

- Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or <http://www.apache.org/licenses/LICENSE-2.0>)
- MIT license ([LICENSE-MIT](LICENSE-MIT) or <http://opensource.org/licenses/MIT>)

at your option.

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any
additional terms or conditions.
