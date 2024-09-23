//! Describes the physical constitution of a device.

use crate::description::util::IterUtil;
use crate::validation::{ValidationError, ValidationErrorType, ValidationObject, ValidationResult};
use crate::values::{non_empty_string, ColorCie, Name};
use serde::de::{Error, Unexpected, Visitor};
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::fmt::{Display, Formatter};
use std::num::NonZeroU8;

/// Describes the physical constitution of the device.
///
/// Corresponds to a `<PhysicalDescriptions>` XML node.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct PhysicalDescriptions {
    /// Describes device emitters.
    ///
    /// Corresponds to the `<Emitters>` XML child node.
    #[serde(
        rename = "Emitters",
        skip_serializing_if = "Vec::is_empty",
        serialize_with = "serialize_emitters",
        deserialize_with = "deserialize_emitters",
        default
    )]
    pub emitters: Vec<Emitter>,

    /// Describes device filters.
    ///
    /// Corresponds to the `<Filters>` XML child node.
    #[serde(
        rename = "Filters",
        skip_serializing_if = "Vec::is_empty",
        serialize_with = "serialize_filters",
        deserialize_with = "deserialize_filters",
        default
    )]
    pub filters: Vec<Filter>,

    /// Describes device default color space.
    ///
    /// Corresponds to the `<ColorSpace>` XML child node.
    #[serde(rename = "ColorSpace")]
    pub color_space: Option<ColorSpace>,

    /// Describes additional device color spaces.
    ///
    /// Corresponds to the `<AdditionalColorSpaces>` XML child node.
    #[serde(
        rename = "AdditionalColorSpaces",
        skip_serializing_if = "Vec::is_empty",
        serialize_with = "serialize_color_spaces",
        deserialize_with = "deserialize_color_spaces",
        default
    )]
    pub additional_color_spaces: Vec<ColorSpace>,

    /// Describes device gamuts.
    ///
    /// Corresponds to the `<Gamuts>` XML child node.
    #[serde(
        rename = "Gamuts",
        skip_serializing_if = "Vec::is_empty",
        serialize_with = "serialize_gamuts",
        deserialize_with = "deserialize_gamuts",
        default
    )]
    pub gamuts: Vec<Gamut>,

    /// Describes nonlinear correlation between DMX input and physical output of a channel.
    ///
    /// Corresponds to the `<DMXProfiles>` XML child node.
    #[serde(
        rename = "DMXProfiles",
        skip_serializing_if = "Vec::is_empty",
        serialize_with = "serialize_dmx_profiles",
        deserialize_with = "deserialize_dmx_profiles",
        default
    )]
    pub dmx_profiles: Vec<DmxProfile>,

    /// Describes color rendering with IES TM-30-15 (99 color samples).
    ///
    /// Corresponds to the `<CRIs>` XML child node.
    #[serde(
        rename = "CRIs",
        skip_serializing_if = "Vec::is_empty",
        serialize_with = "serialize_cri_groups",
        deserialize_with = "deserialize_cri_groups",
        default
    )]
    pub cri_groups: Vec<CriGroup>,

    /// Describes physical connectors of the device.
    ///
    /// Obsolete now, see [WiringObjectGeometry](super::geometry::WiringObjectGeometry).
    ///
    /// Corresponds to the `<Connectors>` XML child node.
    #[serde(
        rename = "Connectors",
        skip_serializing_if = "Vec::is_empty",
        serialize_with = "serialize_connectors",
        deserialize_with = "deserialize_connectors",
        default
    )]
    pub connectors: Vec<Connector>,

    /// Describes physical properties of the device.
    ///
    /// Corresponds to the `<Properties>` XML child node.
    #[serde(rename = "Properties", default)]
    pub properties: Properties,
}

impl PhysicalDescriptions {
    /// Looks up an [Emitter] by [name](Emitter::name).
    pub fn emitter(&self, name: &str) -> Option<&Emitter> {
        self.emitters
            .iter()
            .find(|emitter| emitter.name.as_ref().map(Name::as_ref) == Some(name))
    }

    /// Looks up a [Filter] by [name](Filter::name).
    pub fn filter(&self, name: &str) -> Option<&Filter> {
        self.filters
            .iter()
            .find(|filter| filter.name.as_ref().map(Name::as_ref) == Some(name))
    }

    /// Looks up a [ColorSpace] by [name](ColorSpace::name).
    pub fn color_space(&self, name: &str) -> Option<&ColorSpace> {
        if let Some(color_space) = &self.color_space {
            if color_space.name.as_ref().map(Name::as_ref) == Some(name) {
                return Some(color_space);
            }
        }

        self.additional_color_spaces
            .iter()
            .find(|space| space.name.as_ref().map(Name::as_ref) == Some(name))
    }

    /// Looks up a [Gamut] by [name](Gamut::name).
    pub fn gamut(&self, name: &str) -> Option<&Gamut> {
        self.gamuts
            .iter()
            .find(|gamut| gamut.name.as_ref().map(Name::as_ref) == Some(name))
    }

    /// Looks up a [DmxProfile] by [name](DmxProfile::name).
    pub fn dmx_profile(&self, name: &str) -> Option<&DmxProfile> {
        self.dmx_profiles
            .iter()
            .find(|profile| profile.name.as_ref().map(Name::as_ref) == Some(name))
    }

    /// Looks up a [Connector] by [name](Connector::name).
    pub fn connector(&self, name: &str) -> Option<&Connector> {
        self.connectors
            .iter()
            .find(|connector| connector.name.as_ref().map(Name::as_ref) == Some(name))
    }

    /// Performs validation checks on the object.
    pub fn validate(&self, result: &mut ValidationResult) {
        let duplicate_emitter_name = self
            .emitters
            .iter()
            .filter_map(|emitter| emitter.name.as_ref())
            .find_duplicate();
        if let Some(name) = duplicate_emitter_name {
            result.errors.push(ValidationError::new(
                ValidationObject::Emitter,
                name.to_string(),
                ValidationErrorType::DuplicateName,
            ));
        }

        let duplicate_filter_name = self
            .filters
            .iter()
            .filter_map(|filter| filter.name.as_ref())
            .find_duplicate();
        if let Some(name) = duplicate_filter_name {
            result.errors.push(ValidationError::new(
                ValidationObject::Filter,
                name.to_string(),
                ValidationErrorType::DuplicateName,
            ));
        }

        let duplicate_color_space_name = self
            .color_space
            .iter()
            .chain(self.additional_color_spaces.iter())
            .filter_map(|color_space| color_space.name.as_ref())
            .find_duplicate();
        if let Some(name) = duplicate_color_space_name {
            result.errors.push(ValidationError::new(
                ValidationObject::ColorSpace,
                name.to_string(),
                ValidationErrorType::DuplicateName,
            ));
        }

        let duplicate_gamut_name = self
            .gamuts
            .iter()
            .filter_map(|gamut| gamut.name.as_ref())
            .find_duplicate();
        if let Some(name) = duplicate_gamut_name {
            result.errors.push(ValidationError::new(
                ValidationObject::Gamut,
                name.to_string(),
                ValidationErrorType::DuplicateName,
            ));
        }

        let duplicate_dmx_profile_name = self
            .dmx_profiles
            .iter()
            .filter_map(|dmx_profile| dmx_profile.name.as_ref())
            .find_duplicate();
        if let Some(name) = duplicate_dmx_profile_name {
            result.errors.push(ValidationError::new(
                ValidationObject::DmxProfile,
                name.to_string(),
                ValidationErrorType::DuplicateName,
            ));
        }

        let duplicate_connector_name = self
            .connectors
            .iter()
            .filter_map(|connector| connector.name.as_ref())
            .find_duplicate();
        if let Some(name) = duplicate_connector_name {
            result.errors.push(ValidationError::new(
                ValidationObject::Connector,
                name.to_string(),
                ValidationErrorType::DuplicateName,
            ));
        }

        for emitter in &self.emitters {
            emitter.validate(result);
        }

        for color_space in self
            .color_space
            .iter()
            .chain(self.additional_color_spaces.iter())
        {
            color_space.validate(result);
        }

        for gamut in &self.gamuts {
            gamut.validate(result);
        }

        for dmx_profile in &self.dmx_profiles {
            dmx_profile.validate(result);
        }

        for cri_group in &self.cri_groups {
            cri_group.validate(result);
        }

        for connector in &self.connectors {
            connector.validate(result);
        }
    }
}

define_collect_helper!("Emitter" (serialize_emitters, deserialize_emitters) -> Emitter);

/// Describes an emitter on a fixture.
///
/// Emitters are additive light sources such as LEDs and tungsten lamps with permanently fitted
/// filters.
///
/// Corresponds to an `<Emitter>` XML node.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Emitter {
    /// Unique name of the emitter.
    ///
    /// Corresponds to the `Name` XML attribute.
    #[serde(rename = "@Name", skip_serializing_if = "Option::is_none")]
    pub name: Option<Name>,

    /// Optics of the emitter, either a color or a dominant wavelength.
    #[serde(flatten)]
    pub optic: EmitterOptic,

    /// Optional manufacturer's part number of the diode.
    ///
    /// Corresponds to the `DiodePart` XML attribute.
    #[serde(
        rename = "@DiodePart",
        skip_serializing_if = "Option::is_none",
        deserialize_with = "non_empty_string",
        default
    )]
    pub diode_part: Option<String>,

    /// Measurements describing the relation between the requested output by a control channel and
    /// the physically achieved intensity.
    ///
    /// Corresponds to the `Measurement` XML attribute.
    #[serde(rename = "Measurement", skip_serializing_if = "Vec::is_empty", default)]
    pub measurements: Vec<Measurement>,
}

impl Emitter {
    /// Performs validation checks on the object.
    pub fn validate(&self, result: &mut ValidationResult) {
        if self.name.is_none() {
            result.errors.push(ValidationError::new(
                ValidationObject::Emitter,
                None,
                ValidationErrorType::MissingName,
            ));
        }

        let name = self.name.as_ref();

        if self
            .measurements
            .iter()
            .any(|measurement| measurement.luminous_intensity.is_none())
        {
            result.errors.push(ValidationError::new(
                ValidationObject::Emitter,
                name.map(Name::to_string),
                ValidationErrorType::MissingLuminousIntensity,
            ));
        }
    }
}

/// Defines the optics of an [Emitter].
///
/// An emitter defines either a color or a dominant wavelength. Non-visible emitters (eg. UV)
/// are defined only by a dominant wavelength.
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
#[serde(untagged)]
pub enum EmitterOptic {
    /// Defines the optics of the emitter in terms of a color.
    Color {
        /// Approximate absolute color point.
        ///
        /// Y is relative to the overall output defined in the luminous flux of the related
        /// [BeamGeometry](super::geometry::BeamGeometry) (transmissive case).
        ///
        /// Corresponds to the `Color` XML attribute.
        #[serde(rename = "@Color")]
        color: ColorCie,
        ///// Optional dominant wavelength of the LED.
        /////
        ///// Corresponds to the `DominantWaveLength` XML attribute.
        //#[serde(rename = "@DominantWaveLength", skip_serializing_if = "Option::is_none")]
        //dominant_wave_length: Option<f32>,
    },

    /// Defines the optics of the emitter in terms of a dominant wavelength.
    WaveLength {
        /// Dominant wavelength of the LED.
        ///
        /// Corresponds to the `DominantWaveLength` XML attribute.
        #[serde(rename = "@DominantWaveLength")]
        dominant_wave_length: f32,
    },
}

define_collect_helper!("Filter" (serialize_filters, deserialize_filters) -> Filter);

/// Describes a filter on a fixture.
///
/// Filters are subtractive light sources such as subtractive mixing flags and media used in
/// physical or virtual color wheels.
///
/// Corresponds to a `<Filter>` XML node.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Filter {
    /// Unique name of the filter.
    ///
    /// Corresponds to the `Name` XML attribute.
    #[serde(rename = "@Name", skip_serializing_if = "Option::is_none")]
    pub name: Option<Name>,

    /// Approximate absolute color point when this filter is the only item fully inserted into the
    /// beam and the fixture is at maximum intensity.
    ///
    /// Y is relative to the overall output defined in the luminous flux of the related
    /// [BeamGeometry](super::geometry::BeamGeometry) (transmissive case).
    ///
    /// Corresponds to the `Color` XML attribute.
    #[serde(rename = "@Color")]
    pub color: ColorCie,

    /// Measurements describing the relation between the requested output by a control channel and
    /// the physically achieved intensity.
    ///
    /// Corresponds to all `<Measurement>` XML child nodes.
    #[serde(rename = "Measurement", skip_serializing_if = "Vec::is_empty", default)]
    pub measurements: Vec<Measurement>,
}

impl Filter {
    /// Performs validation checks on the object.
    pub fn validate(&self, result: &mut ValidationResult) {
        if self.name.is_none() {
            result.errors.push(ValidationError::new(
                ValidationObject::Filter,
                None,
                ValidationErrorType::MissingName,
            ));
        }

        let name = self.name.as_ref();

        if self
            .measurements
            .iter()
            .any(|measurement| measurement.transmission.is_none())
        {
            result.errors.push(ValidationError::new(
                ValidationObject::Filter,
                name.map(Name::to_string),
                ValidationErrorType::MissingTransmission,
            ));
        }
    }
}

/// Defines the relation between the requested output by a control channel and the physically
/// achieved intensity.
///
/// The order of measurements corresponds to their ascending physical values.
///
/// Note for additive color mixing: it is assumed that the physical value 0 exists and has zero
/// output.
///
/// Note for subtractive color mixing: the flag is removed with physical value 0 and it does not
/// affect the beam. Physical value 100 is maximally inserted and affects the beam.
///
/// Corresponds to a `<Measurement>` XML node.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Measurement {
    /// A unique value between 0 and 100.
    ///
    ///  - For additive color mixing: emitter intensity DMX percentage.
    ///  - For subtractive color mixing: flag insertion DMX percentage.
    ///
    /// Corresponds to the `Physical` XML attribute.
    #[serde(rename = "@Physical")]
    pub physical: f32,

    /// Overall candela value for the enclosed set of measurement. For additive color mixing only.
    ///
    /// Corresponds to the `LuminousIntensity` XML attribute.
    #[serde(rename = "@LuminousIntensity", skip_serializing_if = "Option::is_none")]
    pub luminous_intensity: Option<f32>,

    /// Total amount of lighting energy passed at this insertion percentage. For subtractive color
    /// mixing only.
    ///
    /// Corresponds to the `Transmission` XML attribute.
    #[serde(rename = "@Transmission", skip_serializing_if = "Option::is_none")]
    pub transmission: Option<f32>,

    /// Interpolation scheme from the previous value.
    ///
    /// Corresponds to the `InterpolationTo` XML attribute.
    #[serde(rename = "@InterpolationTo", default)]
    pub interpolation_to: Interpolation,

    /// An optional list of the energies of specific wavelengths of a spectrum.
    ///
    /// Corresponds to all `<MeasurementPoint>` XML child nodes.
    #[serde(
        rename = "MeasurementPoint",
        skip_serializing_if = "Vec::is_empty",
        default
    )]
    pub points: Vec<MeasurementPoint>,
}

/// Defines the energy of a specific wavelength of a spectrum.
///
/// It is recommended, but not required, that measurement points are evenly spaced. Regions with
/// minimal light energy can be omitted, but the decisive range of spectrum must be included.
/// Recommended measurement spacing is 1nm. Measurement spacing should not exceed 4 nm.
///
/// Corresponds to a `<MeasurementPoint>` XML node.
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct MeasurementPoint {
    /// Center wavelength of measurement (nm).
    ///
    /// Corresponds to the `WaveLength` XML attribute.
    #[serde(rename = "@WaveLength")]
    pub wave_length: f32,

    /// Lighting energy (W/m2/nm).
    ///
    /// Corresponds to the `Energy` XML attribute.
    #[serde(rename = "@Energy")]
    pub energy: f32,
}

/// Interpolation from one [Measurement] to the next.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum Interpolation {
    /// No interpolation.
    Step,

    /// Logarithmic interpolation.
    Log,

    /// Linear interpolation.
    #[default]
    #[serde(other)]
    Linear,
}

define_collect_helper!("ColorSpace" (serialize_color_spaces, deserialize_color_spaces) -> ColorSpace);

/// Defines the color space that is used for color mixing.
///
/// A fixture may be controlled with indirect RGB, Hue/Sat, xyY or CMY control input. The color
/// space indicates how this is converted into physical values.
///
/// Corresponds to a `<ColorSpace>` XML node.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ColorSpace {
    /// Unique name of the color space.
    ///
    /// Corresponds to the `Name` XML attribute.
    #[serde(rename = "@Name", skip_serializing_if = "Option::is_none")]
    pub name: Option<Name>,

    /// Definition of the color space that is used for indirect color mixing.
    ///
    /// Corresponds to the `Mode` XML attribute.
    #[serde(flatten, default)]
    pub mode: ColorSpaceMode,
}

impl ColorSpace {
    /// Performs validation checks on the object.
    pub fn validate(&self, result: &mut ValidationResult) {
        if self.name.is_none() {
            result.errors.push(ValidationError::new(
                ValidationObject::ColorSpace,
                None,
                ValidationErrorType::MissingName,
            ));
        }
    }
}

/// Definition of the [ColorSpace] that is used for indirect color mixing.
///
/// All color spaces are defined in terms of a red primary, green primary, blue primary and white
/// point. A handful of common color spaces are predefined, and others may be defined by providing
/// primary values.
#[derive(Debug, Clone, Copy, Default, Serialize, Deserialize)]
#[serde(tag = "@Mode")]
pub enum ColorSpaceMode {
    /// Custom color space defined in terms of a red primary, green primary, blue primary and
    /// white point.
    Custom {
        /// CIE xyY of the Red Primary.
        ///
        /// Corresponds to the `Red` XML attribute.
        #[serde(rename = "@Red")]
        red: ColorCie,

        /// CIE xyY of the Green Primary.
        ///
        /// Corresponds to the `Green` XML attribute.
        #[serde(rename = "@Green")]
        green: ColorCie,

        /// CIE xyY of the Blue Primary.
        ///
        /// Corresponds to the `Blue` XML attribute.
        #[serde(rename = "@Blue")]
        blue: ColorCie,

        /// CIE xyY of the White Point.
        ///
        /// Corresponds to the `WhitePoint` XML attribute.
        #[serde(rename = "@WhitePoint")]
        white_point: ColorCie,
    },

    /// Kodak ProPhoto ROMM RGB ISO 22028-2:2013 color space.
    ///
    /// Defined as:
    ///  - Red primary: 0.7347, 0.2653
    ///  - Green primary: 0.1596, 0.8404
    ///  - Blue primary: 0.0366, 0.0001
    ///  - White point: 0.3457, 0.3585
    ProPhoto,

    /// ANSI E1.54-2015 color space.
    ///
    /// Defined as:
    ///  - Red primary: 0.7347, 0.2653
    ///  - Green primary: 0.1596, 0.8404
    ///  - Blue primary: 0.0366, 0.001
    ///  - White point: 0.4254, 0.4044
    #[serde(rename = "ANSI")]
    Ansi,

    /// Adobe sRGB, HDTV IEC 61966-2-1:1999 color space.
    ///
    /// Defined as:
    ///  - Red primary: 0.6400, 0.3300, 0.2126
    ///  - Green primary: 0.3000, 0.6000, 0.7152
    ///  - Blue primary: 0.1500, 0.0600, 0.0722
    ///  - White point: 0.3127, 0.3290, 1.0000
    #[default]
    #[serde(other, rename = "sRGB")]
    Srgb,
}

define_collect_helper!("Gamut" (serialize_gamuts, deserialize_gamuts) -> Gamut);

/// Defines the gamut of a fixture.
///
/// A gamut is the set of attainable colors by the fixture.
///
/// Corresponds to a `<Gamut>` XML node.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Gamut {
    /// Unique name of the gamut.
    ///
    /// Corresponds to the `Name` XML attribute.
    #[serde(rename = "@Name", skip_serializing_if = "Option::is_none")]
    pub name: Option<Name>,

    /// Set of points defining the vertices of the gamut's polygon.
    ///
    /// Corresponds to the `Points` XML attribute.
    #[serde(rename = "@Points", skip_serializing_if = "Vec::is_empty")]
    pub points: Vec<ColorCie>, // todo: array separator?
}

impl Gamut {
    /// Performs validation checks on the object.
    pub fn validate(&self, result: &mut ValidationResult) {
        if self.name.is_none() {
            result.errors.push(ValidationError::new(
                ValidationObject::Gamut,
                None,
                ValidationErrorType::MissingName,
            ));
        }

        let name = self.name.as_ref();

        if self.points.len() < 3 {
            result.errors.push(ValidationError::new(
                ValidationObject::Gamut,
                name.map(Name::to_string),
                ValidationErrorType::EmptyPolygon,
            ));
        }
    }
}

define_collect_helper!("DMXProfile" (serialize_dmx_profiles, deserialize_dmx_profiles) -> DmxProfile);

/// Defines a DMX profile description.
///
/// Corresponds to a `<DMXProfile>` XML node.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DmxProfile {
    /// Unique name of the DMX profile.
    ///
    /// Corresponds to the `Name` XML attribute.
    #[serde(rename = "@Name", skip_serializing_if = "Option::is_none")]
    pub name: Option<Name>,

    /// A list of points to define the profile.
    ///
    /// Corresponds to all `<Point>` XML child nodes.
    #[serde(rename = "Point", skip_serializing_if = "Vec::is_empty", default)]
    pub points: Vec<Point>,
}

impl DmxProfile {
    /// Performs validation checks on the object.
    pub fn validate(&self, result: &mut ValidationResult) {
        if self.name.is_none() {
            result.errors.push(ValidationError::new(
                ValidationObject::DmxProfile,
                None,
                ValidationErrorType::MissingName,
            ));
        }

        let name = self.name.as_ref();

        if self.points.is_empty() {
            result.errors.push(ValidationError::new(
                ValidationObject::DmxProfile,
                name.map(Name::to_string),
                ValidationErrorType::EmptyProfile,
            ));
        }

        let duplicate_point = self
            .points
            .iter()
            .map(|point| point.dmx_percentage)
            .find_duplicate();
        if let Some(point) = duplicate_point {
            result.errors.push(ValidationError::new(
                ValidationObject::DmxProfile,
                name.map(Name::to_string),
                ValidationErrorType::DuplicatePoint(point),
            ));
        }
    }
}

/// A point to define the curve of a [DmxProfile].
///
/// Corresponds to a `<Point>` XML node.
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct Point {
    /// DMX percentage of the point.
    ///
    /// Corresponds to the `DMXPercentage` XML attribute.
    #[serde(rename = "@DMXPercentage", default)]
    pub dmx_percentage: f32,

    /// Cubic function coefficient for x^0
    ///
    /// Corresponds to the `CFC0` XML attribute.
    #[serde(rename = "@CFC0", default)]
    pub cfc0: f32,

    /// Cubic function coefficient for x
    ///
    /// Corresponds to the `CFC1` XML attribute.
    #[serde(rename = "@CFC1", default)]
    pub cfc1: f32,

    /// Cubic function coefficient for x^2
    ///
    /// Corresponds to the `CFC2` XML attribute.
    #[serde(rename = "@CFC2", default)]
    pub cfc2: f32,

    /// Cubic function coefficient for x^3
    ///
    /// Corresponds to the `CFC3` XML attribute.
    #[serde(rename = "@CFC3", default)]
    pub cfc3: f32,
}

define_collect_helper!("CRIGroup" (serialize_cri_groups, deserialize_cri_groups) -> CriGroup);

/// Defines the color rendering indices for a single color temperature.
///
/// CRIs (color rendering indices) use TM-30-15 Fidelity Index (Rf) for 99 color samples.
///
/// Corresponds to a `<CRIGroup>` XML node.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CriGroup {
    /// Color temperature in kelvin.
    ///
    /// Corresponds to the `ColorTemperature` XML attribute.
    #[serde(rename = "@ColorTemperature", default = "default_color_temperature")]
    pub color_temperature: f32,

    /// A list of CRI values for up to 99 color samples.
    ///
    /// Corresponds to all `<CRI>` XML child nodes.
    #[serde(rename = "CRI", default)]
    pub cris: Vec<Cri>,
}

impl CriGroup {
    /// Performs validation checks on the object.
    pub fn validate(&self, result: &mut ValidationResult) {
        let duplicate_ces = self.cris.iter().map(|cri| cri.ces).find_duplicate();
        if let Some(ces) = duplicate_ces {
            result.errors.push(ValidationError::new(
                ValidationObject::CriGroup,
                None,
                ValidationErrorType::DuplicateCriSample(ces),
            ));
        }
    }
}

fn default_color_temperature() -> f32 {
    6000.
}

/// Defines the color rendering index for one of the 99 color samples.
///
/// Corresponds to a `<CRI>` XML node.
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct Cri {
    /// Color sample from 1 to 99.
    ///
    /// Corresponds to the `CES` XML attribute.
    #[serde(rename = "@CES")]
    pub ces: Ces,

    /// The color rendering index for this sample.
    ///
    /// Corresponds to the `ColorRenderingIndex` XML attribute.
    #[serde(
        rename = "@ColorRenderingIndex",
        default = "default_color_rendering_index"
    )]
    pub color_rendering_index: u8,
}

/// A color sample identifier.
///
/// Valid identifiers are integers between 1 and 99 inclusive. An instance of this struct is always
/// a valid identifier.
///
/// Serialized format:
/// ```text
/// CES01
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Ces(NonZeroU8);

impl Ces {
    /// Attempts to construct a color sample identifier from an integer.
    ///
    /// This function will fail if the integer is below 1 or above 99.
    pub fn new(value: u8) -> Option<Ces> {
        if value >= 100 {
            return None;
        }
        NonZeroU8::new(value).map(Ces)
    }

    /// Gets the identifier number, which is guaranteed to be between 1 and 99 inclusive.
    pub fn get(self) -> u8 {
        self.0.get()
    }
}

impl Serialize for Ces {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&format!("CES{:0>2}", self.get()))
    }
}

impl<'de> Deserialize<'de> for Ces {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct CesVisitor;

        impl<'de> Visitor<'de> for CesVisitor {
            type Value = Ces;

            fn expecting(&self, formatter: &mut Formatter) -> std::fmt::Result {
                formatter.write_str("a value in the range of `CES01`..`CES99`")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: Error,
            {
                v.strip_prefix("CES")
                    .and_then(|num_str| num_str.parse::<u8>().ok())
                    .and_then(Ces::new)
                    .ok_or_else(|| E::invalid_value(Unexpected::Str(v), &self))
            }
        }

        deserializer.deserialize_str(CesVisitor)
    }
}

impl Display for Ces {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.serialize(f)
    }
}

fn default_color_rendering_index() -> u8 {
    100
}

define_collect_helper!("Connector" (serialize_connectors, deserialize_connectors) -> Connector);

/// Defines physical connectors on a fixture.
///
/// This object is kept for backwards compatibility only. From DIN SPEC 15800:2022 or GDTF v1.2
/// onwards physical connectors are be described as
/// [WiringObjectGeometrys](super::geometry::WiringObjectGeometry) in a fixture's geometry
/// tree.
///
/// Corresponds to a `<Connector>` XML node.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Connector {
    /// Unique name of the connector.
    ///
    /// Corresponds to the `Name` XML attribute.
    #[serde(rename = "@Name", skip_serializing_if = "Option::is_none")]
    pub name: Option<Name>,

    /// The type of the connector.
    ///
    /// Predefined connector types are listed in [Annex D](https://gdtf.eu/gdtf/annex/annex-d/) of
    /// the GDTF specification.
    ///
    /// Corresponds to the `Type` XML attribute.
    #[serde(rename = "@Type")]
    pub type_: Name,

    /// Optionally defines to which DMX Break this connector belongs to.
    ///
    /// Corresponds to the `DMXBreak` XML attribute.
    #[serde(rename = "@DMXBreak", skip_serializing_if = "Option::is_none")]
    pub dmx_break: Option<u32>,

    /// Connectors where the addition of the gender value equal 0 can be connected.
    ///
    /// Male connectors are -1, female are +1, universal are 0.
    ///
    /// Corresponds to the `Gender` XML attribute.
    #[serde(rename = "@Gender", default)]
    pub gender: i32,

    /// Defines the length of the connector's wire in meters.
    ///
    /// A value of `0` means that there is no cable and the connector is built into the housing.
    ///
    /// Corresponds to the `Length` XML attribute.
    #[serde(rename = "@Length", default)]
    pub length: f32,
}

impl Connector {
    /// Performs validation checks on the object.
    pub fn validate(&self, result: &mut ValidationResult) {
        if self.name.is_none() {
            result.errors.push(ValidationError::new(
                ValidationObject::Connector,
                None,
                ValidationErrorType::MissingName,
            ));
        }
    }
}

/// Defines general properties of the device type.
///
/// Corresponds to a `<Properties>` XML node.
#[derive(Debug, Clone, Copy, Default, Serialize, Deserialize)]
pub struct Properties {
    /// Optional temperature range in which the device can be operated.
    ///
    /// Corresponds to the `<OperatingTemperature>` XML child node.
    #[serde(
        rename = "OperatingTemperature",
        skip_serializing_if = "Option::is_none"
    )]
    pub operating_temperature: Option<OperatingTemperature>,

    /// Optional weight of the device including all accessories in kilograms.
    ///
    /// Corresponds to the `<Weight>` XML child node.
    #[serde(
        rename = "Weight",
        skip_serializing_if = "Option::is_none",
        serialize_with = "serialize_value_float",
        deserialize_with = "deserialize_value_float",
        default
    )]
    pub weight: Option<f32>,

    /// Optional height of the legs in meters.
    ///
    /// Defined as the distance between the floor and the bottom base plate.
    ///
    /// Corresponds to the `<LegHeight>` XML child node.
    #[serde(
        rename = "LegHeight",
        skip_serializing_if = "Option::is_none",
        serialize_with = "serialize_value_float",
        deserialize_with = "deserialize_value_float",
        default
    )]
    pub leg_height: Option<f32>,
}

/// Defines the ambient operating temperature range of a device.
///
/// Corresponds to an `<OperatingTemperature>` XML child node.
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct OperatingTemperature {
    /// Lowest temperature the device can be operated in °C.
    ///
    /// Corresponds to the `Low` XML attribute.
    #[serde(rename = "@Low", default = "default_low_operating_temperature")]
    pub low: f32,

    /// Highest temperature the device can be operated in °C.
    ///
    /// Corresponds to the `High` XML attribute.
    #[serde(rename = "@High", default = "default_high_operating_temperature")]
    pub high: f32,
}

fn default_low_operating_temperature() -> f32 {
    0.
}
fn default_high_operating_temperature() -> f32 {
    40.
}

fn serialize_value_float<S>(value: &Option<f32>, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    #[derive(Serialize)]
    struct SeShim<'s> {
        #[serde(rename = "@Value")]
        value: &'s Option<f32>,
    }
    SeShim { value }.serialize(serializer)
}

fn deserialize_value_float<'de, D>(deserializer: D) -> Result<Option<f32>, D::Error>
where
    D: Deserializer<'de>,
{
    #[derive(Deserialize)]
    struct DeShim {
        #[serde(rename = "@Value", default)]
        value: Option<f32>,
    }
    DeShim::deserialize(deserializer).map(|shim| shim.value)
}
