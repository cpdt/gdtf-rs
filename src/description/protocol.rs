//! Protocols supported by a device.

use crate::description::util::IterUtil;
use crate::dmx_mode::DmxMode;
use crate::fixture_type::FixtureType;
use crate::validation::{ValidationError, ValidationErrorType, ValidationObject, ValidationResult};
use crate::values::{Name, Node};
use serde::de::{Error, Unexpected, Visitor};
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::fmt::Formatter;

/// Defines protocols supported by the device.
///
/// If the device supports one or several additional protocols, any protocol specific information
/// is provided.
///
/// Corresponds to a `<Protocols>` XML node.
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Protocols {
    /// Describes RDM information.
    ///
    /// Corresponds to the `<FTRDM>` XML child node.
    #[serde(rename = "FTRDM", skip_serializing_if = "Option::is_none")]
    pub rdm: Option<Rdm>,

    /// Describes Art-Net information.
    ///
    /// Corresponds to the `<Art-Net>` XML child node.
    #[serde(rename = "Art-Net", skip_serializing_if = "Option::is_none")]
    pub art_net: Option<ArtNet>,

    /// Describes sACN information.
    ///
    /// Corresponds to the `<sACN>` XML child node.
    #[serde(rename = "sACN", skip_serializing_if = "Option::is_none")]
    pub sacn: Option<Sacn>,

    /// Describes PosiStageNet information.
    ///
    /// Corresponds to the `<PosiStageNet>` XML child node.
    #[serde(rename = "PosiStageNet", skip_serializing_if = "Option::is_none")]
    pub posi_stage_net: Option<PosiStageNet>,

    /// Describes OpenSoundControl information.
    ///
    /// Corresponds to the `<OpenSoundControl>` XML child node.
    #[serde(rename = "OpenSoundControl", skip_serializing_if = "Option::is_none")]
    pub open_sound_control: Option<OpenSoundControl>,

    /// Describes CITP information.
    ///
    /// Corresponds to the `<CITP>` XML child node.
    #[serde(rename = "CITP", skip_serializing_if = "Option::is_none")]
    pub citp: Option<Citp>,
}

impl Protocols {
    /// Performs validation checks on the object.
    pub fn validate(&self, parent_fixture_type: &FixtureType, result: &mut ValidationResult) {
        if let Some(rdm) = &self.rdm {
            rdm.validate(parent_fixture_type, result);
        }
    }
}

/// Defines corresponding information for devices which support RDM.
///
/// Corresponds to an `<FTRDM>` XML node.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Rdm {
    /// Manufacturer ESTA ID.
    ///
    /// Corresponds to the `ManufacturerID` XML attribute.
    #[serde(
        rename = "@ManufacturerID",
        serialize_with = "serialize_hex",
        deserialize_with = "deserialize_hex"
    )]
    pub manufacturer_id: u32,

    /// Unique device model ID.
    ///
    /// Corresponds to the `DeviceModelID` XML attribute.
    #[serde(
        rename = "@DeviceModelID",
        serialize_with = "serialize_hex",
        deserialize_with = "deserialize_hex"
    )]
    pub device_model_id: u32,

    /// Describes each firmware version the device provides.
    ///
    /// Corresponds to all `<SoftwareVersionID>` XML child nodes.
    #[serde(
        rename = "SoftwareVersionID",
        skip_serializing_if = "Vec::is_empty",
        default
    )]
    pub software_versions: Vec<SoftwareVersion>,
}

impl Rdm {
    /// Performs validation checks on the object.
    pub fn validate(&self, parent_fixture_type: &FixtureType, result: &mut ValidationResult) {
        let duplicate_software_version = self
            .software_versions
            .iter()
            .map(|version| version.value)
            .find_duplicate();
        if let Some(version) = duplicate_software_version {
            result.errors.push(ValidationError::new(
                ValidationObject::SoftwareVersion,
                format!("{:#x}", version),
                ValidationErrorType::DuplicateName,
            ));
        }

        for version in &self.software_versions {
            version.validate(parent_fixture_type, result);
        }
    }
}

/// Defines RDM information for a specific firmware version of a device.
///
/// Corresponds to a `<SoftwareVersionID>` XML node.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct SoftwareVersion {
    /// Software version ID.
    ///
    /// Corresponds to the `Value` XML attribute.
    #[serde(
        rename = "@Value",
        serialize_with = "serialize_hex",
        deserialize_with = "deserialize_hex"
    )]
    pub value: u32,

    /// Personalities supported by the software version.
    ///
    /// Corresponds to all `<DMXPersonality>` XML child nodes.
    #[serde(
        rename = "DMXPersonality",
        skip_serializing_if = "Vec::is_empty",
        default
    )]
    pub personalities: Vec<DmxPersonality>,
}

impl SoftwareVersion {
    /// Performs validation checks on the object.
    pub fn validate(&self, parent_fixture_type: &FixtureType, result: &mut ValidationResult) {
        let duplicate_personality = self
            .personalities
            .iter()
            .map(|personality| personality.value)
            .find_duplicate();
        if let Some(personality) = duplicate_personality {
            result.errors.push(ValidationError::new(
                ValidationObject::DmxPersonality,
                format!("{:#x}", personality),
                ValidationErrorType::DuplicateName,
            ));
        }

        for personality in &self.personalities {
            personality.validate(parent_fixture_type, result);
        }
    }
}

/// Defines DMX personalities supported by an RDM-capable device.
///
/// Corresponds to a `<DMXPersonality>` XML node.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct DmxPersonality {
    /// Hex value of the DMX personality.
    ///
    /// Corresponds to the `Value` XML attribute.
    #[serde(
        rename = "@Value",
        serialize_with = "serialize_hex",
        deserialize_with = "deserialize_hex"
    )]
    pub value: u32,

    /// Link to the DMX mode that can be used with this software version.
    ///
    /// Corresponds to the `DMXMode` XML attribute.
    #[serde(rename = "@DMXMode")]
    pub dmx_mode: Name,
}

impl DmxPersonality {
    /// Looks up the [DmxMode] linked by this DMX personality.
    pub fn dmx_mode<'s>(&self, parent_fixture_type: &'s FixtureType) -> Option<&'s DmxMode> {
        parent_fixture_type.dmx_mode(&self.dmx_mode)
    }

    /// Performs validation checks on the object.
    pub fn validate(&self, parent_fixture_type: &FixtureType, result: &mut ValidationResult) {
        if self.dmx_mode(parent_fixture_type).is_none() {
            result.errors.push(ValidationError::new(
                ValidationObject::DmxPersonality,
                format!("{:#x}", self.value),
                ValidationErrorType::LinkNotFound(
                    ValidationObject::DmxMode,
                    Node::new([self.dmx_mode.clone()]),
                ),
            ));
        }
    }
}

fn serialize_hex<S>(value: &u32, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    serializer.serialize_str(&format!("{value:#x}"))
}

fn deserialize_hex<'de, D>(deserializer: D) -> Result<u32, D::Error>
where
    D: Deserializer<'de>,
{
    struct HexVisitor;

    impl<'de> Visitor<'de> for HexVisitor {
        type Value = u32;

        fn expecting(&self, formatter: &mut Formatter) -> std::fmt::Result {
            formatter.write_str("a hexadecimal integer in the format 0xAAAA")
        }

        fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
        where
            E: Error,
        {
            let Some(hex) = v.strip_prefix("0x") else {
                return Err(E::invalid_value(Unexpected::Str(v), &self));
            };

            u32::from_str_radix(hex, 16).map_err(|_| E::invalid_value(Unexpected::Str(v), &self))
        }
    }

    deserializer.deserialize_str(HexVisitor)
}

/// Defines corresponding information for devices which support Art-Net.
///
/// Corresponds to an `<Art-Net>` XML node.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ArtNet {
    /// Defines custom mappings between Art-Net values and DMX stream values.
    ///
    /// By default all values are mapped 1:1.
    ///
    /// Corresponds to all `<Map>` XML child nodes.
    #[serde(rename = "Map", skip_serializing_if = "Vec::is_empty", default)]
    pub maps: Vec<ProtocolMap>,
}

/// Defines corresponding information for devices which support Streaming ACN.
///
/// Corresponds to a `<sACN>` XML node.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Sacn {
    /// Defines custom mappings between sACN values and DMX stream values.
    ///
    /// By default all values are mapped 1:1.
    ///
    /// Corresponds to all `<Map>` XML child nodes.
    #[serde(rename = "Map", skip_serializing_if = "Vec::is_empty", default)]
    pub maps: Vec<ProtocolMap>,
}

/// Defines a custom mapping between a protocol value and a DMX stream value.
///
/// Corresponds to a `<Map>` XML node.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ProtocolMap {
    /// Value of the protocol value.
    ///
    /// Corresponds to the `Key` XML attribute.
    #[serde(rename = "@Key")]
    pub key: u32,

    /// Value of the DMX value.
    ///
    /// Corresponds to the `Value` XML attribute.
    #[serde(rename = "@Value")]
    pub value: u32,
}

/// Defines corresponding information for devices which support PosiStageNet.
///
/// Has not yet been defined by the GDTF specification.
///
/// Corresponds to a `<PosiStageNet>` XML node.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct PosiStageNet;

/// Defines corresponding information for devices which support OpenSoundControl.
///
/// Has not yet been defined by the GDTF specification.
///
/// Corresponds to an `<OpenSoundControl>` XML node.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct OpenSoundControl;

/// Defines corresponding information for devices which support CITP.
///
/// Has not yet been defined by the GDTF specification.
///
/// Corresponds to a `<CITP>` XML node.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Citp;
