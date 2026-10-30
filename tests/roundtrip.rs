use std::fs::File;
use std::str::FromStr;

use gdtf::{Description, GdtfFile};

/// Serializes the bundled sample file's description and checks it parses back to an equal value.
#[test]
fn sample_description_round_trips() {
    let file = GdtfFile::new(File::open("Generic@RGBW8@test.gdtf").unwrap()).unwrap();
    let xml = file.description.to_string().unwrap();
    let reparsed = Description::from_str(&xml).unwrap();
    assert_eq!(reparsed, file.description);
}

/// Checks that optional attributes and empty collections omitted on write still parse when read.
#[test]
fn omitted_optional_fields_parse() {
    let xml = r#"<GDTF DataVersion="1.2">
  <FixtureType Name="Test" ShortName="T" LongName="Test" Manufacturer="Generic" Description="" FixtureTypeID="00000000-0000-0000-0000-000000000000" RefFT="" Thumbnail="">
    <AttributeDefinitions/>
    <Wheels/>
    <PhysicalDescriptions/>
    <Models/>
    <Geometries/>
    <DMXModes>
      <DMXMode Name="Mode" Geometry="Body">
        <DMXChannels>
          <DMXChannel DMXBreak="1" InitialFunction="" Geometry="Body"/>
        </DMXChannels>
      </DMXMode>
    </DMXModes>
  </FixtureType>
</GDTF>"#;
    let description = Description::from_str(xml).unwrap();
    let channel = &description.fixture_types[0].dmx_modes[0].dmx_channels[0];
    assert_eq!(channel.offset, None);
    assert_eq!(channel.highlight, None);

    let reparsed = Description::from_str(&description.to_string().unwrap()).unwrap();
    assert_eq!(reparsed, description);
}

/// Checks that a fixture type's RefFT reference is read and written back.
#[test]
fn ref_ft_round_trips() {
    let xml = r#"<GDTF DataVersion="1.2">
  <FixtureType Name="Test" ShortName="T" LongName="Test" Manufacturer="Generic" Description="" FixtureTypeID="00000000-0000-0000-0000-000000000000" RefFT="11111111-2222-3333-4444-555555555555" Thumbnail="">
    <AttributeDefinitions/>
    <Wheels/>
    <PhysicalDescriptions/>
    <Models/>
    <Geometries/>
    <DMXModes/>
  </FixtureType>
</GDTF>"#;
    let description = Description::from_str(xml).unwrap();
    assert_eq!(
        description.fixture_types[0].ref_ft.unwrap().to_string(),
        "11111111-2222-3333-4444-555555555555"
    );
    let reparsed = Description::from_str(&description.to_string().unwrap()).unwrap();
    assert_eq!(reparsed, description);
}
