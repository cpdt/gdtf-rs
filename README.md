# gdtf-rs

The General Device Type Format (GDTF) is an open standard for describing devices of the
entertainment industry. The latest version, 1.2, is standardised as
[DIN SPEC 15800:2022](https://www.beuth.de/en/technical-rule/din-spec-15800/349717520).
This crate provides tools to read and inspect GDTF files. This is made up of three parts:
- An object model which closely matches the structure defined in the GDTF specification.
- A fairly lax parser capable of parsing mostly well-formed GDTF files into the object model.
- A small number of utilities for validating and inspecting the object model.
  Importantly, the crate aims to stay close to the GDTF specification. It is not a goal to
  provide a higher-level interface for fixtures represented by a GDTF file.

# Example

```rust
use gdtf_rs::GdtfFile;

let file = std::fs::File::open("Generic@RGBW8@test.gdtf").expect("failed to read file");
let gdtf = GdtfFile::new(file).expect("failed to parse gdtf");
println!("GDTF file defines {} fixture types", gdtf.description.fixture_types.len());
```

# License

Provided under the MIT license.
