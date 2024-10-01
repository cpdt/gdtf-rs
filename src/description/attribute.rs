//! Fixture Type Attributes and associated groupings.
//!
//! Note 1: More information on the definitions of attributes can be found in
//! [Annex A](https://gdtf.eu/gdtf/annex/annex-a/) of the GDTF specification.
//!
//! Note 2: All currently defined Fixture Type Attributes, activation groups, and feature groups can
//! be found in [Annex B](https://gdtf.eu/gdtf/annex/annex-b/) of the GDTF specification.

use crate::description::util::IterUtil;
use crate::validation::{ValidationError, ValidationErrorType, ValidationObject, ValidationResult};
use crate::values::{ColorCie, Name, Node, NodeExt};
use serde::{Deserialize, Serialize};
use std::fmt::{Display, Formatter};

/// Defines all Fixture Type Attributes that are used in the [fixture type](super::fixture_type::FixtureType).
///
/// Corresponds to an `<AttributeDefinitions>` XML node.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct AttributeDefinitions {
    /// Defines which attributes are to be activated together.
    ///
    /// For example, Pan and Tilt are in the same activation group.
    ///
    /// Corresponds to the `<ActivationGroups>` child XML node.
    #[serde(
        rename = "ActivationGroups",
        skip_serializing_if = "Vec::is_empty",
        serialize_with = "serialize_activation_groups",
        deserialize_with = "deserialize_activation_groups",
        default
    )]
    pub activation_groups: Vec<ActivationGroup>,

    /// Describes the logical grouping of attributes.
    ///
    /// For example, Gobo 1 and Gobo 2 are grouped in the feature Gobo of the feature group Gobo.
    ///
    /// Corresponds to the `<FeatureGroups>` child XML node.
    #[serde(
        rename = "FeatureGroups",
        skip_serializing_if = "Vec::is_empty",
        serialize_with = "serialize_feature_groups",
        deserialize_with = "deserialize_feature_groups"
    )]
    pub feature_groups: Vec<FeatureGroup>,

    /// List of Fixture Type Attributes that are used.
    ///
    /// Corresponds to the `<Attributes>` child XML node.
    #[serde(
        rename = "Attributes",
        skip_serializing_if = "Vec::is_empty",
        serialize_with = "serialize_attributes",
        deserialize_with = "deserialize_attributes"
    )]
    pub attributes: Vec<Attribute>,
}

impl AttributeDefinitions {
    /// Looks up an [ActivationGroup] by [name](ActivationGroup::name).
    pub fn activation_group(&self, name: &str) -> Option<&ActivationGroup> {
        self.activation_groups
            .iter()
            .find(|group| group.name.as_ref().map(Name::as_ref) == Some(name))
    }

    /// Looks up a [FeatureGroup] by [name](FeatureGroup::name).
    pub fn feature_group(&self, name: &str) -> Option<&FeatureGroup> {
        self.feature_groups
            .iter()
            .find(|group| group.name.as_ref().map(Name::as_ref) == Some(name))
    }

    /// Looks up an [Attribute] by [name](Attribute::name).
    pub fn attribute(&self, name: &str) -> Option<&Attribute> {
        self.attributes
            .iter()
            .find(|attribute| attribute.name.as_ref().map(Name::as_ref) == Some(name))
    }

    /// Performs validation checks on the object.
    pub fn validate(&self, result: &mut ValidationResult) {
        let duplicate_activation_group_name = self
            .activation_groups
            .iter()
            .filter_map(|group| group.name.as_ref())
            .find_duplicate();
        if let Some(name) = duplicate_activation_group_name {
            result.errors.push(ValidationError::new(
                ValidationObject::ActivationGroup,
                name.to_string(),
                ValidationErrorType::DuplicateName,
            ));
        }

        let duplicate_feature_group_name = self
            .feature_groups
            .iter()
            .filter_map(|group| group.name.as_ref())
            .find_duplicate();
        if let Some(name) = duplicate_feature_group_name {
            result.errors.push(ValidationError::new(
                ValidationObject::FeatureGroup,
                name.to_string(),
                ValidationErrorType::DuplicateName,
            ));
        }

        let duplicate_attribute_name = self
            .attributes
            .iter()
            .filter_map(|attribute| attribute.name.as_ref())
            .find_duplicate();
        if let Some(name) = duplicate_attribute_name {
            result.errors.push(ValidationError::new(
                ValidationObject::Attribute,
                name.to_string(),
                ValidationErrorType::DuplicateName,
            ));
        }

        for activation_group in &self.activation_groups {
            activation_group.validate(result);
        }

        for feature_group in &self.feature_groups {
            feature_group.validate(result);
        }

        for attribute in &self.attributes {
            attribute.validate(self, result);
        }
    }
}

define_collect_helper!("ActivationGroup" (serialize_activation_groups, deserialize_activation_groups) -> ActivationGroup);

/// Defines groups of Fixture Type Attributes that are intended to be used together.
///
/// Example: Usually Pan and Tilt are Fixture Type Attributes that shall be activated together to
/// be able to store and recreate any position.
///
/// Note: All currently defined activation groups can be found in
/// [Annex B](https://gdtf.eu/gdtf/annex/annex-b/) of the GDTF specification.
///
/// Corresponds to an `<ActivationGroup>` XML node.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ActivationGroup {
    /// The unique name of the activation group.
    ///
    /// Corresponds to the `Name` XML attribute.
    #[serde(rename = "@Name", skip_serializing_if = "Option::is_none")]
    pub name: Option<Name>,
}

impl ActivationGroup {
    /// Performs validation checks on the object.
    pub fn validate(&self, result: &mut ValidationResult) {
        if self.name.is_none() {
            result.errors.push(ValidationError::new(
                ValidationObject::ActivationGroup,
                None,
                ValidationErrorType::MissingName,
            ));
        }
    }
}

define_collect_helper!("FeatureGroup" (serialize_feature_groups, deserialize_feature_groups) -> FeatureGroup);

/// Defines the logical grouping of Fixture Type Attributes.
///
/// Note 1: A feature group can contain more than one logical control unit. A feature group Position
/// shall contain PanTilt and XYZ as separate Features.
///
/// Note 2: Usually Pan and Tilt create a logical unit to enable position control, so they must be
/// grouped in a Feature PanTilt.
///
/// Note 3: All currently defined activation groups can be found in
/// [Annex B](https://gdtf.eu/gdtf/annex/annex-b/) of the GDTF specification.
///
/// Corresponds to a `<FeatureGroup>` XML node.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct FeatureGroup {
    /// The unique name of the feature group.
    ///
    /// Corresponds to the `Name` XML attribute.
    #[serde(rename = "@Name", skip_serializing_if = "Option::is_none")]
    pub name: Option<Name>,

    /// The pretty name of the feature group.
    ///
    /// Corresponds to the `Pretty` XML attribute.
    #[serde(rename = "@Pretty")]
    pub pretty: String,

    /// Features contained in the feature group.
    ///
    /// Corresponds to all `<Feature>` child XML nodes.
    #[serde(rename = "Feature", skip_serializing_if = "Vec::is_empty", default)]
    pub features: Vec<Feature>,
}

impl FeatureGroup {
    /// Looks up a [Feature] by [name](Feature::name).
    pub fn feature(&self, name: &str) -> Option<&Feature> {
        self.features
            .iter()
            .find(|feature| feature.name.as_ref().map(|n| n.as_ref()) == Some(name))
    }

    /// Performs validation checks on the object.
    pub fn validate(&self, result: &mut ValidationResult) {
        if self.name.is_none() {
            result.errors.push(ValidationError::new(
                ValidationObject::FeatureGroup,
                None,
                ValidationErrorType::MissingName,
            ));
        }

        let duplicate_feature_name = self
            .features
            .iter()
            .filter_map(|feature| feature.name.as_ref())
            .find_duplicate();
        if let Some(name) = duplicate_feature_name {
            result.errors.push(ValidationError::new(
                ValidationObject::Feature,
                name.to_string(),
                ValidationErrorType::DuplicateName,
            ));
        }

        for feature in &self.features {
            feature.validate(result);
        }
    }
}

/// Defines a feature of a fixture.
///
/// A feature is an element that groups Fixture Type Attributes into a structured way for easier
/// access and search.
///
/// Corresponds to a `<Feature>` XML node.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Feature {
    /// The unique name of the feature.
    ///
    /// Corresponds to the `Name` XML attribute.
    #[serde(rename = "@Name", skip_serializing_if = "Option::is_none")]
    pub name: Option<Name>,
}

impl Feature {
    /// Performs validation checks on the object.
    pub fn validate(&self, result: &mut ValidationResult) {
        if self.name.is_none() {
            result.errors.push(ValidationError::new(
                ValidationObject::Feature,
                None,
                ValidationErrorType::MissingName,
            ));
        }
    }
}

define_collect_helper!("Attribute" (serialize_attributes, deserialize_attributes) -> Attribute);

/// Defines a Fixture Type Attribute.
///
/// An attribute is a singular mutually exclusive control function of a feature, such as Pan or
/// Tilt.
///
/// Definitions of common attributes can be found in [Annex A](https://gdtf.eu/gdtf/annex/annex-a/)
/// of the GDTF specification.
///
/// Corresponds to an `<Attribute>` XML node.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Attribute {
    /// The unique name of the attribute.
    ///
    /// Corresponds to the `Name` XML attribute.
    #[serde(rename = "@Name", skip_serializing_if = "Option::is_none")]
    pub name: Option<Name>,

    /// The pretty name of the attribute.
    ///
    /// Corresponds to the `Pretty` XML attribute.
    #[serde(rename = "@Pretty")]
    pub pretty: String,

    /// Optional link to an [ActivationGroup].
    ///
    /// The link is relative to the parent [AttributeDefinitions] object.
    ///
    /// See the [Attribute::activation_group] method to look up the linked activation group.
    ///
    /// Corresponds to the `ActivationGroup` XML attribute.
    #[serde(rename = "@ActivationGroup", skip_serializing_if = "Option::is_none")]
    pub activation_group: Option<Node>,

    /// Link to the corresponding [Feature].
    ///
    /// The link is relative to the parent [AttributeDefinitions] object.
    ///
    /// See the [Attribute::feature] method to look up the linked feature.
    ///
    /// Corresponds to the `Feature` XML attribute.
    #[serde(rename = "@Feature")]
    pub feature: Option<Node>,

    /// Optional link to the main [Attribute].
    ///
    /// The link is relative to the parent [AttributeDefinitions] object.
    ///
    /// See the [Attribute::main_attribute] method to look up the linked attribute.
    ///
    /// Corresponds to the `MainAttribute` XML attribute.
    #[serde(rename = "@MainAttribute", skip_serializing_if = "Option::is_none")]
    pub main_attribute: Option<Node>,

    /// The physical unit values of the attribute are measured in.
    ///
    /// Corresponds to the `PhysicalUnit` XML attribute.
    #[serde(rename = "@PhysicalUnit")]
    pub physical_unit: PhysicalUnit,

    /// An optional color for the attribute.
    ///
    /// Corresponds to the `Color` XML attribute.
    #[serde(rename = "@Color", skip_serializing_if = "Option::is_none")]
    pub color: Option<ColorCie>,

    /// A list of sub physical units belonging to the attribute.
    ///
    /// Sub physical units define how the attribute controls special aspects of the physical
    /// fixture, such as the offset of an individual gobo or the duty cycle of a strobing shutter.
    ///
    /// Corresponds to all `<SubPhysicalUnit>` XML child nodes.
    #[serde(
        rename = "SubPhysicalUnit",
        skip_serializing_if = "Vec::is_empty",
        default
    )]
    pub subphysical_units: Vec<SubPhysicalUnit>,
}

impl Attribute {
    /// Looks up the optional [ActivationGroup] linked by this attribute.
    pub fn activation_group<'s>(
        &self,
        parent_attribute_definitions: &'s AttributeDefinitions,
    ) -> Option<&'s ActivationGroup> {
        let name = self.activation_group.as_ref()?.single()?;
        parent_attribute_definitions.activation_group(name)
    }

    /// Looks up the corresponding [Feature] linked by this attribute.
    pub fn feature<'s>(
        &self,
        parent_attribute_definitions: &'s AttributeDefinitions,
    ) -> Option<&'s Feature> {
        let (group_name, tail) = self.feature.as_ref()?.split_first()?;
        let feat_name = tail.single()?;
        parent_attribute_definitions
            .feature_group(group_name)?
            .feature(feat_name)
    }

    /// Looks up the optional main [Attribute] linked by this attribute.
    pub fn main_attribute<'s>(
        &self,
        parent_attribute_definitions: &'s AttributeDefinitions,
    ) -> Option<&'s Attribute> {
        let name = self.main_attribute.as_ref()?.single()?;
        parent_attribute_definitions.attribute(name)
    }

    /// Performs validation checks on the object.
    pub fn validate(
        &self,
        parent_attribute_definitions: &AttributeDefinitions,
        result: &mut ValidationResult,
    ) {
        if self.name.is_none() {
            result.errors.push(ValidationError::new(
                ValidationObject::Attribute,
                None,
                ValidationErrorType::MissingName,
            ));
        }

        let name = self.name.as_ref();

        if let (Some(activation_group), None) = (
            &self.activation_group,
            self.activation_group(parent_attribute_definitions),
        ) {
            result.errors.push(ValidationError::new(
                ValidationObject::Attribute,
                name.map(Name::to_string),
                ValidationErrorType::LinkNotFound(
                    ValidationObject::ActivationGroup,
                    activation_group.clone(),
                ),
            ));
        }

        if let (Some(feature), None) = (&self.feature, self.feature(parent_attribute_definitions)) {
            result.errors.push(ValidationError::new(
                ValidationObject::Attribute,
                name.map(Name::to_string),
                ValidationErrorType::LinkNotFound(ValidationObject::Feature, feature.clone()),
            ));
        }

        if let (Some(main_attribute), None) = (
            &self.main_attribute,
            self.main_attribute(parent_attribute_definitions),
        ) {
            result.errors.push(ValidationError::new(
                ValidationObject::Attribute,
                name.map(Name::to_string),
                ValidationErrorType::LinkNotFound(
                    ValidationObject::Attribute,
                    main_attribute.clone(),
                ),
            ));
        }

        let duplicate_sub_physical_type = self
            .subphysical_units
            .iter()
            .map(|unit| unit.type_)
            .find_duplicate();
        if let Some(sub_physical_unit_type) = duplicate_sub_physical_type {
            result.errors.push(ValidationError::new(
                ValidationObject::Attribute,
                name.map(Name::to_string),
                ValidationErrorType::DuplicateSubPhysicalUnit(sub_physical_unit_type),
            ));
        }
    }
}

/// Describes how an attribute relates to a sub physical unit of a fixture.
///
/// Sub physical units define how the attribute controls special aspects of the physical
/// fixture, such as the offset of an individual gobo or the duty cycle of a strobing shutter.
///
/// Corresponds to a `<SubPhysicalUnit>` XML node.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SubPhysicalUnit {
    /// The type of sub physical unit.
    ///
    /// Corresponds to the `Type` XML attribute.
    #[serde(rename = "@Type")]
    pub type_: SubPhysicalUnitType,

    /// The physical unit values of the unit are measured in.
    ///
    /// Corresponds to the `PhysicalUnit` XML attribute.
    #[serde(rename = "@PhysicalUnit", default)]
    pub physical_unit: PhysicalUnit,

    /// The default physical from of the subphysical unit.
    ///
    /// Corresponds to the `PhysicalFrom` XML attribute.
    #[serde(rename = "@PhysicalFrom", default = "physical_from_default")]
    pub physical_from: f64,

    /// The default physical to of the subphysical unit.
    ///
    /// Corresponds to the `PhysicalTo` XML attribute.
    #[serde(rename = "@PhysicalTo", default = "physical_to_default")]
    pub physical_to: f64,
}

/// Valid types of a [SubPhysicalUnit].
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub enum SubPhysicalUnitType {
    /// Defines the offset of the selected wheel slot in degrees.
    ///
    /// Defined for attributes: `Gobo(n)`, `Gobo(n)SelectSpin`, `Gobo(n)SelectShake`,
    /// `Gobo(n)WheelIndex`, `Gobo(n)WheelSpin`, `Gobo(n)WheelShake`, `Gobo(n)WheelRandom`,
    /// `Gobo(n)WheelAudio`, `AnimationWheel(n)`, `AnimationWheel(n)Audio`,
    /// `AnimationWheel(n)Random`, `AnimationWheel(n)SelectShake`, `AnimationWheel(n)SelectSpin`,
    /// `Color(n)`, `Color(n)WheelIndex`, `Color(n)WheelSpin`, `Color(n)WheelRandom`,
    /// `Color(n)WheelAudio`.
    PlacementOffset,

    /// Defines the peak amplitude of an effect as a percentage of the size.
    ///
    /// Defined for attributes: `Gobo(n)SelectShake`, `Gobo(n)WheelShake`, `Gobo(n)PosShake`,
    /// `AnimationWheel(n)SelectShake`, `AnimationWheel(n)PosShake`, `AnimationSystem(n)PosShake`.
    Amplitude,

    /// Defines the minimum position in relation to the whole way of the spline.
    ///
    /// Defined for attributes: `AnimationSystem(n)Ramp`, `AnimationSystem(n)Shake`.
    AmplitudeMin,

    /// Defines the maximum position in relation to the whole way of the spline.
    ///
    /// Defined for attributes: `AnimationSystem(n)Ramp`, `AnimationSystem(n)Shake`.
    AmplitudeMax,

    /// Defines the duration of the on time of the effect.
    ///
    /// Defined for attributes: `Shutter(n)Strobe`, `Shutter(n)StrobeRandom`, `IrisStrobe`,
    /// `IrisStrobeRandom`.
    Duration,

    /// Defines the duration percentage of one period in which the effect is on.
    ///
    /// Defined for attributes: `AnimationSystem(n)Ramp`, `Shutter(n)StrobePulse`,
    /// `Shutter(n)StrobePulseClose`, `Shutter(n)StrobePulseOpen`, `IrisPulseClose`,
    /// `IrisPulseOpen`, `Frost(n)PulseOpen`, `Frost(n)PulseClose`, `Frost(n)Ramp`.
    DutyCycle,

    /// Defines the offset of the end of the end of the effect from the start as a percentage of the
    /// total period.
    ///
    /// Defined for attributes: `Shutter(n)Strobe`, `Shutter(n)StrobePulse`,
    /// `Shutter(n)StrobePulseClose`, `Shutter(n)StrobePulseOpen`, `IrisStrobe`, `IrisPulseClose`,
    /// `IrisPulseOpen`, `Frost(n)PulseOpen`, `Frost(n)PulseClose`, `Frost(n)Ramp`.
    TimeOffset,

    /// Defines the minimum percentage to which the iris closes.
    ///
    /// Defined for attributes: `IrisStrobe`, `IrisStrobeRandom`, `IrisPulseClose`, `IrisPulseOpen`,
    /// `IrisRandomPulseClose`, `IrisRandomPulseOpen`.
    MinimumOpening,

    /// No defined behaviour.
    Value,

    /// Defines the size of the beam compared to the original size.
    ///
    /// Defined for attributes: `BeamShaper`.
    RatioHorizontal,

    /// Defines the size of the beam compared to the original size.
    ///
    /// Defined for attributes: `BeamShaper`.
    RatioVertical,
}

impl SubPhysicalUnitType {
    /// Returns a string representation of the unit type.
    pub fn as_str(self) -> &'static str {
        match self {
            SubPhysicalUnitType::PlacementOffset => "PlacementOffset",
            SubPhysicalUnitType::Amplitude => "Amplitude",
            SubPhysicalUnitType::AmplitudeMin => "AmplitudeMin",
            SubPhysicalUnitType::AmplitudeMax => "AmplitudeMax",
            SubPhysicalUnitType::Duration => "Duration",
            SubPhysicalUnitType::DutyCycle => "DutyCycle",
            SubPhysicalUnitType::TimeOffset => "TimeOffset",
            SubPhysicalUnitType::MinimumOpening => "MinimumOpening",
            SubPhysicalUnitType::Value => "Value",
            SubPhysicalUnitType::RatioHorizontal => "RatioHorizontal",
            SubPhysicalUnitType::RatioVertical => "RatioVertical",
        }
    }
}

impl Display for SubPhysicalUnitType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.serialize(f)
    }
}

fn physical_from_default() -> f64 {
    0.
}
fn physical_to_default() -> f64 {
    1.
}

/// A measure of physical quantity.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum PhysicalUnit {
    /// Measured in percentage (%).
    Percent,

    /// Measured in meters (m).
    Length,

    /// Measured in kilograms (kg).
    Mass,

    /// Measured in seconds (s).
    Time,

    /// Measured in kelvin (K).
    Temperature,

    /// Measured in candela (cd).
    LuminousIntensity,

    /// Measured in degrees.
    Angle,

    /// Measured in newtons (N).
    Force,

    /// Measured in hertz (Hz).
    Frequency,

    /// Measured in amps (A).
    Current,

    /// Measured in voltage (V).
    Voltage,

    /// Measured in watts (W).
    Power,

    /// Measured in joules (J).
    Energy,

    /// Measured in meters squared (m2).
    Area,

    /// Measured in meters cubed (m3).
    Volume,

    /// Measured in meters per second (m/s).
    Speed,

    /// Measured in meters per second per second (m/s2).
    Acceleration,

    /// Measured in degrees per second (degree/s).
    AngularSpeed,

    /// Measured in degrees per second per second (degree/s2).
    AngularAccc,

    /// Measured in nanometers (nm).
    WaveLength,

    /// Measured as abstract intensity of a color from 0-1.
    ColorComponent,

    /// No physical unit.
    #[default]
    #[serde(other)]
    None,
}
