//! Describes all DMX modes of a device.

use crate::attribute::{Attribute, SubPhysicalUnit};
use crate::description::util::IterUtil;
use crate::fixture_type::FixtureType;
use crate::geometry::Geometry;
use crate::physical_descriptions::{ColorSpace, DmxProfile, Emitter, Filter, Gamut};
use crate::validation::{ValidationError, ValidationErrorType, ValidationObject, ValidationResult};
use crate::values::{non_empty_string, DmxValue, Name, Node, NodeExt};
use crate::wheel::{Wheel, WheelSlot};
use serde::de::value::StrDeserializer;
use serde::de::{Error, Unexpected, Visitor};
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::fmt::Formatter;

/// Describes the logical control of the device in a specific mode.
///
/// If a device firmware revision changes a DMX footprint, then such revisions are specified as a
/// new DMX mode.
///
/// Corresponds to a `<DMXMode>` XML node.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DmxMode {
    /// The unique name of the DMX mode.
    ///
    /// Corresponds to the `Name` XML attribute.
    #[serde(rename = "@Name", skip_serializing_if = "Option::is_none")]
    pub name: Option<Name>,

    /// Description of the DMX mode.
    ///
    /// Corresponds to the `Description` XML attribute.
    #[serde(rename = "@Description", default)]
    pub description: String,

    /// Name of the first geometry in the device.
    ///
    /// Only top level geometries are allowed to be linked.
    ///
    /// Corresponds to the `Geometry` XML attribute.
    #[serde(rename = "@Geometry")]
    pub geometry: Option<Name>,

    /// Description of all DMX channels used in this mode.
    ///
    /// Corresponds to the `<DMXChannels>` XML child node.
    #[serde(
        rename = "DMXChannels",
        skip_serializing_if = "Vec::is_empty",
        serialize_with = "serialize_dmx_channels",
        deserialize_with = "deserialize_dmx_channels"
    )]
    pub dmx_channels: Vec<DmxChannel>,

    /// Description of relations between channels.
    ///
    /// Corresponds to the `<Relations>` XML child node.
    #[serde(
        rename = "Relations",
        skip_serializing_if = "Vec::is_empty",
        serialize_with = "serialize_relations",
        deserialize_with = "deserialize_relations",
        default
    )]
    pub relations: Vec<Relation>,

    /// Used to describe macros of the manufacturer.
    ///
    /// Corresponds to the `<FTMacros>` XML child node.
    #[serde(
        rename = "FTMacros",
        skip_serializing_if = "Vec::is_empty",
        serialize_with = "serialize_ft_macros",
        deserialize_with = "deserialize_ft_macros",
        default
    )]
    pub ft_macros: Vec<FtMacro>,
}

impl DmxMode {
    /// Looks up the [Geometry] linked by this DMX mode.
    pub fn geometry<'s>(&self, parent_fixture_type: &'s FixtureType) -> Option<&'s Geometry> {
        parent_fixture_type.root_geometry(self.geometry.as_ref()?)
    }

    /// Looks up a [DmxChannel] by [name](DmxChannel::name).
    pub fn dmx_channel(&self, name: &str) -> Option<&DmxChannel> {
        self.dmx_channels
            .iter()
            .find(|channel| channel.name().as_ref() == name)
    }

    /// Looks up a [Relation] by [name](Relation::name).
    pub fn relation(&self, name: &str) -> Option<&Relation> {
        self.relations
            .iter()
            .find(|relation| relation.name.as_ref().map(Name::as_ref) == Some(name))
    }

    /// Looks up an [FtMacro] by [name](FtMacro::name).
    pub fn ft_macro(&self, name: &str) -> Option<&FtMacro> {
        self.ft_macros
            .iter()
            .find(|m| m.name.as_ref().map(Name::as_ref) == Some(name))
    }

    /// Performs validation checks on the object.
    pub fn validate(&self, parent_fixture_type: &FixtureType, result: &mut ValidationResult) {
        if self.name.is_none() {
            result.errors.push(ValidationError::new(
                ValidationObject::DmxMode,
                None,
                ValidationErrorType::MissingName,
            ));
        }

        let name = self.name.as_ref();

        if let (Some(geometry), None) = (&self.geometry, self.geometry(parent_fixture_type)) {
            result.errors.push(ValidationError::new(
                ValidationObject::DmxMode,
                name.map(Name::to_string),
                ValidationErrorType::LinkNotFound(
                    ValidationObject::Geometry,
                    Node::new([geometry.clone()]),
                ),
            ));
        }

        let duplicate_channel_name = self
            .dmx_channels
            .iter()
            .map(|channel| channel.name())
            .find_duplicate();
        if let Some(name) = duplicate_channel_name {
            result.errors.push(ValidationError::new(
                ValidationObject::DmxChannel,
                name.to_string(),
                ValidationErrorType::DuplicateName,
            ));
        }

        let duplicate_relation_name = self
            .relations
            .iter()
            .filter_map(|relation| relation.name.as_ref())
            .find_duplicate();
        if let Some(name) = duplicate_relation_name {
            result.errors.push(ValidationError::new(
                ValidationObject::Relation,
                name.to_string(),
                ValidationErrorType::DuplicateName,
            ));
        }

        let duplicate_macro_name = self
            .ft_macros
            .iter()
            .filter_map(|ft_macro| ft_macro.name.as_ref())
            .find_duplicate();
        if let Some(name) = duplicate_macro_name {
            result.errors.push(ValidationError::new(
                ValidationObject::FtMacro,
                name.to_string(),
                ValidationErrorType::DuplicateName,
            ));
        }

        for dmx_channel in &self.dmx_channels {
            dmx_channel.validate(parent_fixture_type, self, result);
        }

        for relation in &self.relations {
            relation.validate(self, result);
        }

        for ft_macro in &self.ft_macros {
            ft_macro.validate(self, result);
        }
    }
}

define_collect_helper!("DMXChannel" (serialize_dmx_channels, deserialize_dmx_channels) -> DmxChannel);

/// Defines a DMX channel in the footprint of a device.
///
/// Corresponds to a `<DMXChannel>` XML node.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DmxChannel {
    /// Number of the DMX break.
    ///
    /// Corresponds to the `DMXBreak` XML attribute.
    #[serde(rename = "@DMXBreak", default)]
    pub dmx_break: DmxBreak,

    /// Relative addresses of the current DMX channel from highest to least significant.
    ///
    /// A value of None indicates the channel does not have any addresses.
    ///
    /// Corresponds to the `Offset` XML attribute.
    #[serde(
        rename = "@Offset",
        skip_serializing_if = "Option::is_none",
        serialize_with = "serialize_dmx_channel_offset",
        deserialize_with = "deserialize_dmx_channel_offset"
    )]
    pub offset: Option<Vec<i32>>,

    /// Link to the channel function that will be activated by default for this channel.
    ///
    /// Corresponds to the `InitialFunction` XML attribute.
    #[serde(rename = "@InitialFunction", skip_serializing_if = "Option::is_none")]
    pub initial_function: Option<Node>,

    /// Highlight value for the current channel.
    ///
    /// A value of None indicates the channel does not respond to highlight commands.
    ///
    /// Corresponds to the `Highlight` XML attribute.
    #[serde(
        rename = "@Highlight",
        skip_serializing_if = "Option::is_none",
        serialize_with = "serialize_dmx_channel_highlight",
        deserialize_with = "deserialize_dmx_channel_highlight"
    )]
    pub highlight: Option<DmxValue>,

    /// Name of the geometry the current channel controls.
    ///
    /// The Geometry should be the place in the tree of geometries where the function of the DMX
    /// channel (as defined by [ChannelFunction]) is located either physically or logically.
    /// If the DMX channel doesn't have a location, it will be in the top level geometry of the
    /// geometry tree. Attributes follow a trickle down principle, so they are inherited from top
    /// down.
    ///
    /// Corresponds to the `Geometry` XML attribute.
    #[serde(rename = "@Geometry")]
    pub geometry: Name,

    /// A list of mutually exclusive logical channels that make up the DMX channel.
    ///
    /// Corresponds to all `<LogicalChannel>` XML child nodes.
    #[serde(
        rename = "LogicalChannel",
        skip_serializing_if = "Vec::is_empty",
        default
    )]
    pub logical_channels: Vec<LogicalChannel>,
}

impl DmxChannel {
    /// Returns the unique name of the DMX channel.
    ///
    /// The name of a DMX channel is not user-defined and consists of the linked geometry name and
    /// the attribute name of the first logical channel, separated by a `_` character. For example
    /// a DMX channel linked to a geometry named `Yoke` with a logical channel connected to the
    /// `Pan` attribute, is named `Yoke_Pan`.
    ///
    /// This combination must be unique within the containing [DmxMode].
    pub fn name(&self) -> Name {
        let logical_channel_name = self
            .logical_channels
            .first()
            .map(|channel| channel.name().as_ref())
            .unwrap_or("");
        Name::new(format!("{}_{}", self.geometry, logical_channel_name)).unwrap()
    }

    /// Looks up the [ChannelFunction] that is activated by default for this channel.
    ///
    /// If an initial function is not specified by the channel, it defaults to the first channel
    /// function of the first logical function of this DMX channel.
    pub fn initial_function(&self) -> Option<(&LogicalChannel, &ChannelFunction)> {
        let Some(node) = &self.initial_function else {
            let first_channel = self.logical_channels.first()?;
            let first_function = first_channel.channel_functions.first()?;
            return Some((first_channel, first_function));
        };

        // first name in the node is the name of this DmxChannel (for some reason)
        let (channel_name, tail) = node.split_first()?;
        if channel_name != &self.name() {
            return None;
        }

        let (logical_channel_name, tail) = tail.split_first()?;
        let logical_channel = self.logical_channel(logical_channel_name)?;

        let function_name = tail.single()?;
        let function = logical_channel.channel_function(function_name)?;

        Some((logical_channel, function))
    }

    /// Looks up the [Geometry] linked by this DMX channel.
    pub fn geometry<'s>(&self, parent_fixture_type: &'s FixtureType) -> Option<&'s Geometry> {
        parent_fixture_type.nested_geometry(&self.geometry)
    }

    /// Looks up a [LogicalChannel] by [name](LogicalChannel::name).
    pub fn logical_channel(&self, name: &str) -> Option<&LogicalChannel> {
        self.logical_channels
            .iter()
            .find(|channel| channel.name().as_ref() == name)
    }

    /// Performs validation checks on the object.
    pub fn validate(
        &self,
        parent_fixture_type: &FixtureType,
        parent_dmx_mode: &DmxMode,
        result: &mut ValidationResult,
    ) {
        if let (Some(initial_function), None) = (&self.initial_function, self.initial_function()) {
            result.errors.push(ValidationError::new(
                ValidationObject::DmxChannel,
                self.name().to_string(),
                ValidationErrorType::LinkNotFound(
                    ValidationObject::ChannelFunction,
                    initial_function.clone(),
                ),
            ));
        }

        if self.geometry(parent_fixture_type).is_none() {
            result.errors.push(ValidationError::new(
                ValidationObject::DmxChannel,
                self.name().to_string(),
                ValidationErrorType::LinkNotFound(
                    ValidationObject::Geometry,
                    Node::new([self.geometry.clone()]),
                ),
            ))
        }

        let duplicate_logical_channel_name = self
            .logical_channels
            .iter()
            .map(|logical_channel| logical_channel.name())
            .find_duplicate();
        if let Some(name) = duplicate_logical_channel_name {
            result.errors.push(ValidationError::new(
                ValidationObject::LogicalChannel,
                name.to_string(),
                ValidationErrorType::DuplicateName,
            ));
        }

        for logical_channel in &self.logical_channels {
            logical_channel.validate(parent_fixture_type, parent_dmx_mode, result);
        }
    }
}

/// Number of the DMX break specified by a [DmxChannel], or the special "Overwrite" value.
///
/// Serialized format:
/// ```text
/// Normal value:  1
/// Special value: Overwrite
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DmxBreak {
    /// Defined DMX break.
    Value(i32),

    /// Indicates this number will be overwritten by a
    /// [ReferenceGeometry](super::geometry::ReferenceGeometry).
    Overwrite,
}

impl Serialize for DmxBreak {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match *self {
            DmxBreak::Value(value) => serializer.serialize_i32(value),
            DmxBreak::Overwrite => serializer.serialize_str("Overwrite"),
        }
    }
}

impl<'de> Deserialize<'de> for DmxBreak {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct DmxBreakVisitor;

        impl<'de> Visitor<'de> for DmxBreakVisitor {
            type Value = DmxBreak;

            fn expecting(&self, formatter: &mut Formatter) -> std::fmt::Result {
                formatter.write_str("`Overwrite` or an integer")
            }

            fn visit_i32<E>(self, v: i32) -> Result<Self::Value, E>
            where
                E: Error,
            {
                Ok(DmxBreak::Value(v))
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: Error,
            {
                if v == "Overwrite" {
                    return Ok(DmxBreak::Overwrite);
                }

                match v.parse::<i32>() {
                    Ok(value) => Ok(DmxBreak::Value(value)),
                    Err(_) => Err(E::invalid_value(Unexpected::Str(v), &self)),
                }
            }
        }

        deserializer.deserialize_str(DmxBreakVisitor)
    }
}

impl Default for DmxBreak {
    fn default() -> Self {
        DmxBreak::Value(1)
    }
}

fn serialize_dmx_channel_offset<S>(
    value: &Option<Vec<i32>>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    match value {
        None => serializer.serialize_str("None"),
        Some(values) => serializer.serialize_str(&values.iter().join(",")),
    }
}

fn deserialize_dmx_channel_offset<'de, D>(deserializer: D) -> Result<Option<Vec<i32>>, D::Error>
where
    D: Deserializer<'de>,
{
    let s: String = Deserialize::deserialize(deserializer)?;
    let s = s.trim();
    if s == "None" || s.is_empty() {
        return Ok(None);
    }

    let values: Result<Vec<_>, _> = s
        .split(',')
        .map(|value| {
            value.parse::<i32>().map_err(|_| {
                D::Error::invalid_value(
                    Unexpected::Str(value),
                    &"an integer between -2^31 and 2^31",
                )
            })
        })
        .collect();
    Ok(Some(values?))
}

fn serialize_dmx_channel_highlight<S>(
    value: &Option<DmxValue>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    match value {
        None => serializer.serialize_str("None"),
        Some(value) => value.serialize(serializer),
    }
}

fn deserialize_dmx_channel_highlight<'de, D>(deserializer: D) -> Result<Option<DmxValue>, D::Error>
where
    D: Deserializer<'de>,
{
    struct DmxHighlightVisitor;

    impl<'de> Visitor<'de> for DmxHighlightVisitor {
        type Value = Option<DmxValue>;

        fn expecting(&self, formatter: &mut Formatter) -> std::fmt::Result {
            formatter.write_str("`None` or a DMX value in the format uint/n or uint/ns")
        }

        fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
        where
            E: Error,
        {
            if v == "None" {
                Ok(None)
            } else {
                Ok(Some(DmxValue::deserialize(StrDeserializer::new(v))?))
            }
        }
    }

    deserializer.deserialize_str(DmxHighlightVisitor)
}

/// Defines a logical, mutually exclusive part of a [DmxChannel].
///
/// Each [Attribute] is assigned to a logical channel and defines the function of the logical
/// channel.
///
/// All logical channels that are children of the same DMX channel are mutually exclusive.
///
/// In a DMX mode, only one logical channel with the same attribute can reference the same geometry
/// at a time.
///
/// Corresponds to a `<LogicalChannel>` XML node.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LogicalChannel {
    /// Link to the attribute.
    ///
    /// Corresponds to the `Attribute` XML attribute.
    #[serde(rename = "@Attribute")]
    pub attribute: Node,

    /// Controls if the logical channel fades between values.
    ///
    /// - With snap enabled, the channel will jump directly to the new value.
    /// - With snap disabled, the channel will fade between values.
    ///
    /// Corresponds to the `Snap` XML attribute.
    #[serde(
        rename = "@Snap",
        skip_serializing_if = "skip_serializing_logical_channel_snap",
        serialize_with = "serialize_logical_channel_snap",
        deserialize_with = "deserialize_logical_channel_snap",
        default
    )]
    pub snap: bool,

    /// Defines if all the subordinate channel functions react to a Group Control defined by the
    /// control system.
    ///
    /// Corresponds to the `Master` XML attribute.
    #[serde(rename = "@Master", default)]
    pub master: LogicalChannelMaster,

    /// Minimum fade time for moves in black action.
    ///
    /// `mib_fade` is defined for the complete DMX range.
    ///
    /// Corresponds to the `MibFade` XML attribute.
    #[serde(rename = "@MibFade", default)]
    pub mib_fade: f32,

    /// Minimum fade time for the subordinate channel functions to change DMX values by the control
    /// system.
    ///
    /// `dmx_change_time_limit` is defined for the complete DMX range.
    ///
    /// Corresponds to the `DMXChangeTimeLimit` XML attribute.
    #[serde(rename = "@DMXChangeTimeLimit", default)]
    pub dmx_change_time_limit: f32,

    /// A list of channel functions defining the function of each DMX range.
    ///
    /// Corresponds to all `<ChannelFunction>` XML child nodes.
    #[serde(
        rename = "ChannelFunction",
        skip_serializing_if = "Vec::is_empty",
        default
    )]
    pub channel_functions: Vec<ChannelFunction>,
}

impl LogicalChannel {
    /// Returns the unique name of the logical channel.
    ///
    /// The name of a logical channel is not user-defined and is equal to the linked attribute name.
    pub fn name(&self) -> &Name {
        self.attribute.first().unwrap()
    }

    /// Looks up the [Attribute] linked by this logical channel.
    pub fn attribute<'s>(&self, parent_fixture_type: &'s FixtureType) -> Option<&'s Attribute> {
        let name = self.attribute.single()?;
        parent_fixture_type.attribute_definitions.attribute(name)
    }

    /// Looks up a [ChannelFunction] by [name](ChannelFunction::name).
    pub fn channel_function(&self, name: &str) -> Option<&ChannelFunction> {
        self.channel_functions
            .iter()
            .find(|func| func.name.as_ref().map(Name::as_ref) == Some(name))
    }

    /// Performs validation checks on the object.
    pub fn validate(
        &self,
        parent_fixture_type: &FixtureType,
        parent_dmx_mode: &DmxMode,
        result: &mut ValidationResult,
    ) {
        if self.attribute(parent_fixture_type).is_none() {
            result.errors.push(ValidationError::new(
                ValidationObject::LogicalChannel,
                self.name().to_string(),
                ValidationErrorType::LinkNotFound(
                    ValidationObject::Attribute,
                    self.attribute.clone(),
                ),
            ));
        }

        let duplicate_channel_function_name = self
            .channel_functions
            .iter()
            .filter_map(|channel_function| channel_function.name.as_ref())
            .find_duplicate();
        if let Some(name) = duplicate_channel_function_name {
            result.errors.push(ValidationError::new(
                ValidationObject::ChannelFunction,
                name.to_string(),
                ValidationErrorType::DuplicateName,
            ));
        }

        for channel_function in &self.channel_functions {
            channel_function.validate(parent_fixture_type, parent_dmx_mode, result);
        }
    }
}

fn skip_serializing_logical_channel_snap(value: &bool) -> bool {
    // `false` is the default, no need to serialize
    !*value
}

fn serialize_logical_channel_snap<S>(value: &bool, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    serializer.serialize_str(match value {
        true => "Yes",
        false => "No",
    })
}

fn deserialize_logical_channel_snap<'de, D>(deserializer: D) -> Result<bool, D::Error>
where
    D: Deserializer<'de>,
{
    struct SnapVisitor;

    impl<'de> Visitor<'de> for SnapVisitor {
        type Value = bool;

        fn expecting(&self, formatter: &mut Formatter) -> std::fmt::Result {
            formatter.write_str("`Yes`, `No`, `On` or `Off`")
        }

        fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
        where
            E: Error,
        {
            match v {
                "Yes" | "On" => Ok(true),
                "No" | "Off" => Ok(false),
                _ => Err(E::invalid_value(Unexpected::Str(v), &self)),
            }
        }
    }

    deserializer.deserialize_str(SnapVisitor)
}

/// Defines if all the channel functions in a [LogicalChannel] react to a Group Control defined by
/// the control system.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum LogicalChannelMaster {
    /// Logical channel reacts to a grand master.
    Grand,

    /// Logical channel reacts to a group master.
    Group,

    /// Logical channel does not react to a master.
    #[default]
    #[serde(other)]
    None,
}

/// Defines the function of a DMX range in a [LogicalChannel].
///
/// Corresponds to a `<ChannelFunction>` XML node.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChannelFunction {
    /// The unique name of the channel function.
    ///
    /// When a name is not supplied, it is assumed to be equal to the name of the attribute and the
    /// number of the channel function.
    ///
    /// Corresponds to the `Name` XML attribute.
    #[serde(rename = "@Name", skip_serializing_if = "Option::is_none")]
    pub name: Option<Name>,

    /// Link to the attribute.
    ///
    /// Corresponds to the `Attribute` XML attribute.
    #[serde(rename = "@Attribute", default = "default_channel_function_attribute")]
    pub attribute: Node,

    /// The manufacturer's original name of the attribute.
    ///
    /// Corresponds to the `OriginalAttribute` XML attribute.
    #[serde(rename = "@OriginalAttribute", default)]
    pub original_attribute: String,

    /// Start DMX value.
    ///
    /// The end DMX value is calculated as the `dmx_from` of the next channel function - 1, or the
    /// maximum value of the DMX channel.
    ///
    /// Corresponds to the `DMXFrom` XML attribute.
    #[serde(rename = "@DMXFrom", default)]
    pub dmx_from: DmxValue,

    /// Default DMX value of the channel function when activated by the control system.
    ///
    /// Corresponds to the `Default` XML attribute.
    #[serde(rename = "@Default", default)]
    pub default: DmxValue,

    /// Physical start value.
    ///
    /// Corresponds to the `PhysicalFrom` XML attribute.
    #[serde(
        rename = "@PhysicalFrom",
        default = "default_channel_function_physical_from"
    )]
    pub physical_from: f32,

    /// Physical end value.
    ///
    /// Corresponds to the `PhysicalTo` XML attribute.
    #[serde(
        rename = "@PhysicalTo",
        default = "default_channel_function_physical_to"
    )]
    pub physical_to: f32,

    /// Time in seconds to move from min to max of the channel function.
    ///
    /// Corresponds to the `RealFade` XML attribute.
    #[serde(rename = "@RealFade", default)]
    pub real_fade: f32,

    /// Time in seconds to accelerate from stop to maximum velocity.
    ///
    /// Corresponds to the `RealAcceleration` XML attribute.
    #[serde(rename = "@RealAcceleration", default)]
    pub real_acceleration: f32,

    /// Optional link to a wheel.
    ///
    /// Corresponds to the `Wheel` XML attribute.
    #[serde(rename = "@Wheel", skip_serializing_if = "Option::is_none")]
    pub wheel: Option<Node>,

    /// Optional link to an emitter in the
    /// [PhysicalDescriptions](super::physical_descriptions::PhysicalDescriptions).
    ///
    /// Corresponds to the `Emitter` XML attribute.
    #[serde(rename = "@Emitter", skip_serializing_if = "Option::is_none")]
    pub emitter: Option<Node>,

    /// Optional link to a filter in the
    /// [PhysicalDescriptions](super::physical_descriptions::PhysicalDescriptions).
    ///
    /// Corresponds to the `Filter` XML attribute.
    #[serde(rename = "@Filter", skip_serializing_if = "Option::is_none")]
    pub filter: Option<Node>,

    /// Optional link to a color space in the
    /// [PhysicalDescriptions](super::physical_descriptions::PhysicalDescriptions).
    ///
    /// Corresponds to the `ColorSpace` XML attribute.
    #[serde(rename = "@ColorSpace", skip_serializing_if = "Option::is_none")]
    pub color_space: Option<Node>,

    /// Optional link to a gamut in the
    /// [PhysicalDescriptions](super::physical_descriptions::PhysicalDescriptions).
    ///
    /// Corresponds to the `Gamut` XML attribute.
    #[serde(rename = "@Gamut", skip_serializing_if = "Option::is_none")]
    pub gamut: Option<Node>,

    /// Optional link to a [DmxChannel] or a [ChannelFunction].
    ///
    /// Corresponds to the `ModeMaster` XML attribute.
    #[serde(flatten, skip_serializing_if = "Option::is_none")]
    pub mode_master: Option<ModeMasterNode>,

    /// Optional link to a DMX profile.
    ///
    /// Corresponds to the `DMXProfile` XML attribute.
    #[serde(rename = "@DMXProfile", skip_serializing_if = "Option::is_none")]
    pub dmx_profile: Option<Node>,

    /// Minimum physical value that will be used for the DMX profile.
    ///
    /// When omitted the value of `physical_from` should be used.
    ///
    /// Corresponds to the `Min` XML attribute.
    #[serde(rename = "@Min", skip_serializing_if = "Option::is_none")]
    pub min: Option<f32>,

    /// Maximum physical value that will be used for the DMX profile.
    ///
    /// When omitted the value of `physical_to` should be used.
    ///
    /// Corresponds to the `Max` XML attribute.
    #[serde(rename = "@Max", skip_serializing_if = "Option::is_none")]
    pub max: Option<f32>,

    /// Custom name that can be used to address this channel function with other command-based
    /// protocols like OSC.
    ///
    /// When omitted the name is calculated as the full node name of the channel function, for
    /// example `Head_Dimmer.Dimmer.Dimmer`.
    ///
    /// Corresponds to the `CustomName` XML attribute.
    #[serde(
        rename = "@CustomName",
        skip_serializing_if = "Option::is_none",
        deserialize_with = "non_empty_string",
        default
    )]
    pub custom_name: Option<String>,

    /// The list of channel sets in the channel function.
    ///
    /// Corresponds to all `<ChannelSet>` XML child nodes.
    #[serde(rename = "ChannelSet", skip_serializing_if = "Vec::is_empty", default)]
    pub channel_sets: Vec<ChannelSet>,

    /// The list of sub channel sets in the channel function.
    ///
    /// Corresponds to all `<SubChannelSet>` XML child nodes.
    #[serde(
        rename = "SubChannelSet",
        skip_serializing_if = "Vec::is_empty",
        default
    )]
    pub sub_channel_sets: Vec<SubChannelSet>,
}

impl ChannelFunction {
    /// Looks up the [Attribute] linked by this channel function.
    pub fn attribute<'s>(&self, parent_fixture_type: &'s FixtureType) -> Option<&'s Attribute> {
        let name = self.attribute.single()?;
        parent_fixture_type.attribute_definitions.attribute(name)
    }

    /// Looks up the [Wheel] linked by this channel function.
    pub fn wheel<'s>(&self, parent_fixture_type: &'s FixtureType) -> Option<&'s Wheel> {
        let name = self.wheel.as_ref()?.single()?;
        parent_fixture_type.wheel(name)
    }

    /// Looks up the [Emitter] linked by this channel function.
    pub fn emitter<'s>(&self, parent_fixture_type: &'s FixtureType) -> Option<&'s Emitter> {
        let name = self.emitter.as_ref()?.single()?;
        parent_fixture_type.physical_descriptions.emitter(name)
    }

    /// Looks up the [Filter] linked by this channel function.
    pub fn filter<'s>(&self, parent_fixture_type: &'s FixtureType) -> Option<&'s Filter> {
        let name = self.filter.as_ref()?.single()?;
        parent_fixture_type.physical_descriptions.filter(name)
    }

    /// Looks up the [ColorSpace] linked by this channel function.
    pub fn color_space<'s>(&self, parent_fixture_type: &'s FixtureType) -> Option<&'s ColorSpace> {
        let name = self.color_space.as_ref()?.single()?;
        parent_fixture_type.physical_descriptions.color_space(name)
    }

    /// Looks up the [Gamut] linked by this channel function.
    pub fn gamut<'s>(&self, parent_fixture_type: &'s FixtureType) -> Option<&'s Gamut> {
        let name = self.gamut.as_ref()?.single()?;
        parent_fixture_type.physical_descriptions.gamut(name)
    }

    /// Looks up the [DmxProfile] linked by this channel function.
    pub fn dmx_profile<'s>(&self, parent_fixture_type: &'s FixtureType) -> Option<&'s DmxProfile> {
        let name = self.dmx_profile.as_ref()?.single()?;
        parent_fixture_type.physical_descriptions.dmx_profile(name)
    }

    /// Returns the minimum physical value that will be used for the DMX profile, or the physical
    /// start value if a minimum is not set.
    pub fn min(&self) -> f32 {
        self.min.unwrap_or(self.physical_from)
    }

    /// Returns the maximum physical value that will be used for the DMX profile, or the physical
    /// end value if a maximum is not set.
    pub fn max(&self) -> f32 {
        self.max.unwrap_or(self.physical_to)
    }

    /// Looks up a [ChannelSet] by [name](ChannelSet::name).
    pub fn channel_set(&self, name: &str) -> Option<&ChannelSet> {
        self.channel_sets
            .iter()
            .find(|channel_set| channel_set.name.as_ref().map(Name::as_ref) == Some(name))
    }

    /// Looks up a [SubChannelSet] by [name](SubChannelSet::name).
    pub fn sub_channel_set(&self, name: &str) -> Option<&SubChannelSet> {
        self.sub_channel_sets
            .iter()
            .find(|sub| sub.name.as_ref().map(Name::as_ref) == Some(name))
    }

    /// Performs validation checks on the object.
    pub fn validate(
        &self,
        parent_fixture_type: &FixtureType,
        parent_dmx_mode: &DmxMode,
        result: &mut ValidationResult,
    ) {
        if self.name.is_none() {
            result.errors.push(ValidationError::new(
                ValidationObject::ChannelFunction,
                None,
                ValidationErrorType::MissingName,
            ));
        }

        let name = self.name.as_ref();

        if self.attribute(parent_fixture_type).is_none() {
            result.errors.push(ValidationError::new(
                ValidationObject::ChannelFunction,
                name.map(Name::to_string),
                ValidationErrorType::LinkNotFound(
                    ValidationObject::Attribute,
                    self.attribute.clone(),
                ),
            ));
        }

        if let (Some(wheel), None) = (&self.wheel, self.wheel(parent_fixture_type)) {
            result.errors.push(ValidationError::new(
                ValidationObject::ChannelFunction,
                name.map(Name::to_string),
                ValidationErrorType::LinkNotFound(ValidationObject::Wheel, wheel.clone()),
            ));
        }

        if let (Some(emitter), None) = (&self.emitter, self.emitter(parent_fixture_type)) {
            result.errors.push(ValidationError::new(
                ValidationObject::ChannelFunction,
                name.map(Name::to_string),
                ValidationErrorType::LinkNotFound(ValidationObject::Emitter, emitter.clone()),
            ));
        }

        if let (Some(filter), None) = (&self.filter, self.filter(parent_fixture_type)) {
            result.errors.push(ValidationError::new(
                ValidationObject::ChannelFunction,
                name.map(Name::to_string),
                ValidationErrorType::LinkNotFound(ValidationObject::Filter, filter.clone()),
            ));
        }

        if let (Some(color_space), None) =
            (&self.color_space, self.color_space(parent_fixture_type))
        {
            result.errors.push(ValidationError::new(
                ValidationObject::ChannelFunction,
                name.map(Name::to_string),
                ValidationErrorType::LinkNotFound(
                    ValidationObject::ColorSpace,
                    color_space.clone(),
                ),
            ));
        }

        if let (Some(gamut), None) = (&self.gamut, self.gamut(parent_fixture_type)) {
            result.errors.push(ValidationError::new(
                ValidationObject::ChannelFunction,
                name.map(Name::to_string),
                ValidationErrorType::LinkNotFound(ValidationObject::Gamut, gamut.clone()),
            ));
        }

        if let Some(mode_master) = &self.mode_master {
            if mode_master.mode_master(parent_dmx_mode).is_none() {
                result.errors.push(ValidationError::new(
                    ValidationObject::ChannelFunction,
                    name.map(Name::to_string),
                    ValidationErrorType::LinkNotFound(
                        ValidationObject::ModeMaster,
                        mode_master.node.clone(),
                    ),
                ));
            }
        }

        if let (Some(dmx_profile), None) =
            (&self.dmx_profile, self.dmx_profile(parent_fixture_type))
        {
            result.errors.push(ValidationError::new(
                ValidationObject::ChannelFunction,
                name.map(Name::to_string),
                ValidationErrorType::LinkNotFound(
                    ValidationObject::DmxProfile,
                    dmx_profile.clone(),
                ),
            ));
        }

        for channel_set in &self.channel_sets {
            channel_set.validate(parent_fixture_type, self, result);
        }

        for sub_channel_set in &self.sub_channel_sets {
            sub_channel_set.validate(parent_fixture_type, result);
        }
    }
}

fn default_channel_function_attribute() -> Node {
    Node::new(vec![Name::new("NoFeature").unwrap()])
}
fn default_channel_function_physical_from() -> f32 {
    0.
}
fn default_channel_function_physical_to() -> f32 {
    1.
}

/// A link to a [DmxChannel] or [ChannelFunction] defining the mode master of a [ChannelFunction].
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModeMasterNode {
    /// Link to a [DmxChannel] or a [ChannelFunction].
    ///
    /// Corresponds to the `ModeMaster` XML attribute.
    #[serde(rename = "@ModeMaster")]
    pub node: Node,

    /// DMX start value.
    ///
    /// Corresponds to the `ModeFrom` XML attribute.
    #[serde(rename = "@ModeFrom", default)]
    pub from: DmxValue,

    /// DMX end value.
    ///
    /// Corresponds to the `ModeTo` XML attribute.
    #[serde(rename = "@ModeTo", default)]
    pub to: DmxValue,
}

impl ModeMasterNode {
    /// Looks up the [DmxChannel] or [ChannelFunction] linked by this channel function.
    pub fn mode_master<'s>(&self, parent_dmx_mode: &'s DmxMode) -> Option<ModeMaster<'s>> {
        let (channel_name, tail) = self.node.split_first()?;
        let channel = parent_dmx_mode.dmx_channel(channel_name)?;

        let Some((logical_channel_name, tail)) = tail.split_first() else {
            return Some(ModeMaster::DmxChannel(channel));
        };

        let logical_channel = channel.logical_channel(logical_channel_name)?;
        let function_name = tail.single()?;
        let function = logical_channel.channel_function(function_name)?;

        Some(ModeMaster::ChannelFunction(
            channel,
            logical_channel,
            function,
        ))
    }
}

/// Represents the [DmxChannel] or [ChannelFunction] linked as a mode master with a [ModeMasterNode].
#[derive(Debug, Clone, Copy)]
pub enum ModeMaster<'s> {
    /// Mode master is a DMX channel.
    DmxChannel(&'s DmxChannel),

    /// Mode master is a channel function.
    ChannelFunction(&'s DmxChannel, &'s LogicalChannel, &'s ChannelFunction),
}

/// Defines a channel set of a [ChannelFunction].
///
/// Corresponds to a `<ChannelSet>` XML node.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChannelSet {
    /// The name of the channel set.
    ///
    /// Corresponds to the `Name` XML attribute.
    #[serde(rename = "@Name", skip_serializing_if = "Option::is_none")]
    pub name: Option<Name>,

    /// Start DMX value.
    ///
    /// The end DMX value is calculated as the `dmx_from` of the next channel set - 1, or the
    /// maximum value of the current channel function.
    ///
    /// Corresponds to the `DMXFrom` XML attribute.
    #[serde(rename = "@DMXFrom")]
    pub dmx_from: DmxValue,

    /// Physical start value.
    ///
    /// When omitted the value of the parent channel function's `physical_from` should be used.
    ///
    /// Corresponds to the `PhysicalFrom` XML attribute.
    #[serde(rename = "@PhysicalFrom", skip_serializing_if = "Option::is_none")]
    pub physical_from: Option<f32>,

    /// Physical end value.
    ///
    /// When omitted the value of the parent channel function's `physical_to` should be used.
    ///
    /// Corresponds to the `PhysicalTo` XML attribute.
    #[serde(rename = "@PhysicalTo", skip_serializing_if = "Option::is_none")]
    pub physical_to: Option<f32>,

    /// If the channel function has a link to a wheel, specifies the corresponding slot index.
    ///
    /// The wheel slot index results from the order of slots of the wheel which is linked in the
    /// channel function.
    ///
    /// Corresponds to the `WheelSlotIndex` XML attribute.
    #[serde(
        rename = "@WheelSlotIndex",
        skip_serializing_if = "Option::is_none",
        serialize_with = "serialize_wheel_slot_index",
        deserialize_with = "deserialize_wheel_slot_index",
        default
    )]
    pub wheel_slot_index: Option<i32>,
}

impl ChannelSet {
    /// Returns the physical start value of the channel set, or the physical start value of the
    /// parent channel function if no value is provided.
    pub fn physical_from(&self, parent_channel_function: &ChannelFunction) -> f32 {
        self.physical_from
            .unwrap_or(parent_channel_function.physical_from)
    }

    /// Returns the physical end value of the channel set, or the physical end value of the parent
    /// channel function if no value is provided.
    pub fn physical_to(&self, parent_channel_function: &ChannelFunction) -> f32 {
        self.physical_to
            .unwrap_or(parent_channel_function.physical_to)
    }

    /// Looks up the [WheelSlot] linked by this channel slot, in the [Wheel] linked by the parent
    /// channel function.
    pub fn wheel_slot<'s>(&self, parent_wheel: &'s Wheel) -> Option<&'s WheelSlot> {
        parent_wheel.slots.get(self.wheel_slot_index? as usize)
    }

    /// Performs validation checks on the object.
    pub fn validate(
        &self,
        parent_fixture_type: &FixtureType,
        parent_channel_function: &ChannelFunction,
        result: &mut ValidationResult,
    ) {
        let name = self.name.as_ref();

        if let (Some(wheel_slot_index), Some(wheel)) = (
            self.wheel_slot_index,
            parent_channel_function.wheel(parent_fixture_type),
        ) {
            if self.wheel_slot(wheel).is_none() {
                result.errors.push(ValidationError::new(
                    ValidationObject::ChannelSet,
                    name.map(Name::to_string),
                    ValidationErrorType::LinkNotFound(
                        ValidationObject::WheelSlot,
                        Node::new([Name::new_lossy(wheel_slot_index.to_string())]),
                    ),
                ));
            }
        }
    }
}

fn serialize_wheel_slot_index<S>(value: &Option<i32>, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    let serialize_value = value.map_or(0, |val| val.max(0) + 1);
    serialize_value.serialize(serializer)
}

fn deserialize_wheel_slot_index<'de, D>(deserializer: D) -> Result<Option<i32>, D::Error>
where
    D: Deserializer<'de>,
{
    let serialize_value = i32::deserialize(deserializer)?;
    if serialize_value <= 0 {
        Ok(None)
    } else {
        Ok(Some(serialize_value - 1))
    }
}

/// Defines a sub channel set of a [ChannelFunction].
///
/// Corresponds to a `<SubChannelSet>` XML node.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SubChannelSet {
    /// The name of the sub channel set.
    ///
    /// Corresponds to the `Name` XML attribute.
    #[serde(rename = "@Name", skip_serializing_if = "Option::is_none")]
    pub name: Option<Name>,

    /// Physical start value.
    ///
    /// Corresponds to the `PhysicalFrom` XML attribute.
    #[serde(rename = "@PhysicalFrom")]
    pub physical_from: f32,

    /// Physical end value.
    ///
    /// Corresponds to the `PhysicalTo` XML attribute.
    #[serde(rename = "@PhysicalTo")]
    pub physical_to: f32,

    /// Link to the sub physical unit.
    ///
    /// Corresponds to the `SubPhysicalUnit` XML attribute.
    #[serde(rename = "@SubPhysicalUnit")]
    pub sub_physical_unit: Node,

    /// Optional link to the DMX profile.
    ///
    /// Corresponds to the `DMXProfile` XML attribute.
    #[serde(rename = "@DMXProfile", skip_serializing_if = "Option::is_none")]
    pub dmx_profile: Option<Node>,
}

impl SubChannelSet {
    /// Looks up the [SubPhysicalUnit] linked by this sub channel set.
    pub fn sub_physical_unit<'s>(
        &self,
        parent_fixture_type: &'s FixtureType,
    ) -> Option<&'s SubPhysicalUnit> {
        let (attribute_name, tail) = self.sub_physical_unit.split_first()?;
        let attribute = parent_fixture_type
            .attribute_definitions
            .attribute(attribute_name)?;

        let sub_name = tail.single()?;
        attribute
            .subphysical_units
            .iter()
            .find(|unit| unit.type_.as_str() == sub_name.as_ref())
    }

    /// Looks up the [DmxProfile] linked by this sub channel set.
    pub fn dmx_profile<'s>(&self, parent_fixture_type: &'s FixtureType) -> Option<&'s DmxProfile> {
        let name = self.dmx_profile.as_ref()?.single()?;
        parent_fixture_type.physical_descriptions.dmx_profile(name)
    }

    /// Performs validation checks on the object.
    pub fn validate(&self, parent_fixture_type: &FixtureType, result: &mut ValidationResult) {
        if self.name.is_none() {
            result.errors.push(ValidationError::new(
                ValidationObject::SubChannelSet,
                None,
                ValidationErrorType::MissingName,
            ));
        }

        let name = self.name.as_ref();

        if self.sub_physical_unit(parent_fixture_type).is_none() {
            result.errors.push(ValidationError::new(
                ValidationObject::SubChannelSet,
                name.map(Name::to_string),
                ValidationErrorType::LinkNotFound(
                    ValidationObject::SubPhysicalUnit,
                    self.sub_physical_unit.clone(),
                ),
            ));
        }

        if let (Some(dmx_profile), None) =
            (&self.dmx_profile, self.dmx_profile(parent_fixture_type))
        {
            result.errors.push(ValidationError::new(
                ValidationObject::SubChannelSet,
                name.map(Name::to_string),
                ValidationErrorType::LinkNotFound(
                    ValidationObject::DmxProfile,
                    dmx_profile.clone(),
                ),
            ));
        }
    }
}

define_collect_helper!("Relation" (serialize_relations, deserialize_relations) -> Relation);

/// Describes a dependency between a DMX channel and a channel function.
///
/// Corresponds to a `<Relation>` XML node.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Relation {
    /// The unique name of the relation.
    ///
    /// Corresponds to the `Name` XML attribute.
    #[serde(rename = "@Name", skip_serializing_if = "Option::is_none")]
    pub name: Option<Name>,

    /// Link to the master DMX channel.
    ///
    /// Corresponds to the `Master` XML attribute.
    #[serde(rename = "@Master")]
    pub master: Node,

    /// Link to the following channel function.
    ///
    /// Corresponds to the `Follower` XML attribute.
    #[serde(rename = "@Follower")]
    pub follower: Node,

    /// Type of the relation.
    ///
    /// Corresponds to the `Type` XML attribute.
    #[serde(rename = "@Type")]
    pub type_: RelationType,
}

impl Relation {
    /// Looks up the master [DmxChannel] linked by this relation.
    pub fn master<'s>(&self, parent_dmx_mode: &'s DmxMode) -> Option<&'s DmxChannel> {
        let name = self.master.single()?;
        parent_dmx_mode.dmx_channel(name)
    }

    /// Looks up the follower [ChannelFunction] linked by this relation.
    pub fn follower<'s>(
        &self,
        parent_dmx_mode: &'s DmxMode,
    ) -> Option<(&'s DmxChannel, &'s LogicalChannel, &'s ChannelFunction)> {
        let (channel_name, tail) = self.follower.split_first()?;
        let channel = parent_dmx_mode.dmx_channel(channel_name)?;

        let (logical_name, tail) = tail.split_first()?;
        let logical = channel.logical_channel(logical_name)?;

        let func_name = tail.single()?;
        let func = logical.channel_function(func_name)?;

        Some((channel, logical, func))
    }

    /// Performs validation checks on the object.
    pub fn validate(&self, parent_dmx_mode: &DmxMode, result: &mut ValidationResult) {
        if self.name.is_none() {
            result.errors.push(ValidationError::new(
                ValidationObject::Relation,
                None,
                ValidationErrorType::MissingName,
            ));
        }

        let name = self.name.as_ref();

        if self.master(parent_dmx_mode).is_none() {
            result.errors.push(ValidationError::new(
                ValidationObject::Relation,
                name.map(Name::to_string),
                ValidationErrorType::LinkNotFound(
                    ValidationObject::DmxChannel,
                    self.master.clone(),
                ),
            ));
        }

        if self.follower(parent_dmx_mode).is_none() {
            result.errors.push(ValidationError::new(
                ValidationObject::Relation,
                name.map(Name::to_string),
                ValidationErrorType::LinkNotFound(
                    ValidationObject::ChannelFunction,
                    self.follower.clone(),
                ),
            ));
        }
    }
}

/// Type of relation described in a [Relation].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum RelationType {
    /// Master multiplies the value of the follower.
    Multiply,

    /// Master overrides the value of the follower.
    Override,
}

define_collect_helper!("FTMacro" (serialize_ft_macros, deserialize_ft_macros) -> FtMacro);

/// Describes a macro to be executed by the control system.
///
/// A macro is made up of several sequences of DMX values.
///
/// Corresponds to an `<FTMacro>` XML node.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FtMacro {
    /// The unique name of the macro.
    ///
    /// Corresponds to the `Name` XML attribute.
    #[serde(rename = "@Name", skip_serializing_if = "Option::is_none")]
    pub name: Option<Name>,

    /// Optional link to a channel function.
    ///
    /// Corresponds to the `ChannelFunction` XML attribute.
    #[serde(rename = "@ChannelFunction", skip_serializing_if = "Option::is_none")]
    pub channel_function: Option<Node>,

    /// A list of DMX sequences.
    ///
    /// Corresponds to all `<MacroDMX>` XML child nodes.
    #[serde(rename = "MacroDMX", skip_serializing_if = "Vec::is_empty", default)]
    pub dmx: Vec<MacroDmx>,
}

impl FtMacro {
    /// Looks up the [ChannelFunction] linked by this macro.
    pub fn channel_function<'s>(
        &self,
        parent_dmx_mode: &'s DmxMode,
    ) -> Option<(&'s DmxChannel, &'s LogicalChannel, &'s ChannelFunction)> {
        let (channel_name, tail) = self.channel_function.as_ref()?.split_first()?;
        let channel = parent_dmx_mode.dmx_channel(channel_name)?;

        let (logical_name, tail) = tail.split_first()?;
        let logical = channel.logical_channel(logical_name)?;

        let function_name = tail.single()?;
        let function = logical.channel_function(function_name)?;

        Some((channel, logical, function))
    }

    /// Performs validation checks on the object.
    pub fn validate(&self, parent_dmx_mode: &DmxMode, result: &mut ValidationResult) {
        if self.name.is_none() {
            result.errors.push(ValidationError::new(
                ValidationObject::FtMacro,
                None,
                ValidationErrorType::MissingName,
            ));
        }

        let name = self.name.as_ref();

        if let (Some(channel_function), None) = (
            &self.channel_function,
            self.channel_function(parent_dmx_mode),
        ) {
            result.errors.push(ValidationError::new(
                ValidationObject::FtMacro,
                name.map(Name::to_string),
                ValidationErrorType::LinkNotFound(
                    ValidationObject::ChannelFunction,
                    channel_function.clone(),
                ),
            ));
        }

        let dmx_values = self
            .dmx
            .iter()
            .flat_map(|dmx| dmx.steps.iter())
            .flat_map(|step| step.values.iter());
        for dmx_value in dmx_values {
            if dmx_value.dmx_channel(parent_dmx_mode).is_none() {
                result.errors.push(ValidationError::new(
                    ValidationObject::FtMacro,
                    name.map(Name::to_string),
                    ValidationErrorType::LinkNotFound(
                        ValidationObject::DmxChannel,
                        dmx_value.dmx_channel.clone(),
                    ),
                ));
            }
        }
    }
}

/// Describes an individual sequence of DMX values in an [FtMacro].
///
/// Corresponds to a `<MacroDMX>` XML node.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MacroDmx {
    /// The steps in the sequence.
    ///
    /// Corresponds to all `<MacroDMXStep>` XML child nodes.
    #[serde(
        rename = "MacroDMXStep",
        skip_serializing_if = "Vec::is_empty",
        default
    )]
    pub steps: Vec<MacroDmxStep>,
}

/// Describes a step in a [MacroDmx] sequence.
///
/// Corresponds to a `<MacroDMXStep>` XML node.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MacroDmxStep {
    /// Duration of the step in seconds.
    ///
    /// Corresponds to the `Duration` XML attribute.
    #[serde(rename = "@Duration", default = "default_macro_dmx_step_duration")]
    pub duration: f32,

    /// Values to apply in this step.
    ///
    /// Corresponds to all `<MacroDMXValue>` XML child nodes.
    #[serde(
        rename = "MacroDMXValue",
        skip_serializing_if = "Vec::is_empty",
        default
    )]
    pub values: Vec<MacroDmxValue>,
}

fn default_macro_dmx_step_duration() -> f32 {
    1.
}

/// Describes the value for a DMX channel in a [MacroDmxStep].
///
/// Corresponds to a `<MacroDMXValue>` XML node.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MacroDmxValue {
    /// Value of the DMX channel.
    ///
    /// Corresponds to the `Value` XML attribute.
    #[serde(rename = "@Value")]
    pub value: DmxValue,

    /// Link to the DMX channel.
    ///
    /// Corresponds to the `DMXChannel` XML attribute.
    #[serde(rename = "@DMXChannel")]
    pub dmx_channel: Node,
}

impl MacroDmxValue {
    /// Looks up the [DmxChannel] linked by this macro.
    pub fn dmx_channel<'s>(&self, parent_dmx_mode: &'s DmxMode) -> Option<&'s DmxChannel> {
        let name = self.dmx_channel.single()?;
        parent_dmx_mode.dmx_channel(name)
    }
}
