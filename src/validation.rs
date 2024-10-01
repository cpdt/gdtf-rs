use crate::attribute::SubPhysicalUnitType;
use crate::physical_descriptions::Ces;
use crate::values::{Node, Version};
use std::fmt::{Display, Formatter};

/// Result of performing validation checks on a GDTF file.
#[derive(Debug, Clone, Default)]
pub struct ValidationResult {
    /// Errors that were encountered during validation.
    ///
    /// Validation errors do not impact the integrity of the file as a whole, but they may cause
    /// some features to not function as expected.
    pub errors: Vec<ValidationError>,
}

impl ValidationResult {
    /// Constructs an empty validation result.
    pub fn new() -> Self {
        Self::default()
    }
}

/// An error that was encountered during validation.
#[derive(Debug, Clone)]
pub struct ValidationError {
    /// The kind of object where the error occurred.
    pub object: ValidationObject,

    /// The name of the object where the error occurred.
    pub name: Option<String>,

    /// The type and details of the error.
    pub ty: ValidationErrorType,
}

impl ValidationError {
    /// Constructs a validation error.
    pub fn new(
        object: ValidationObject,
        name: impl Into<Option<String>>,
        ty: ValidationErrorType,
    ) -> Self {
        ValidationError {
            object,
            name: name.into(),
            ty,
        }
    }
}

impl Display for ValidationError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.object)?;
        if let Some(name) = &self.name {
            write!(f, " `{name}`")?;
        }
        write!(f, ": {}", self.ty)
    }
}

/// Details on the type of error that occurred.
#[derive(Debug, Clone)]
pub enum ValidationErrorType {
    /// Invalid GDTF version.
    InvalidVersion(Version),

    /// Object is missing name.
    MissingName,

    /// Two of the same kind of object have the same name.
    DuplicateName,

    /// An attribute has two sub physical units with the same type.
    DuplicateSubPhysicalUnit(SubPhysicalUnitType),

    /// An object that was linked to could not be found.
    LinkNotFound(ValidationObject, Node),

    /// A thumbnail resource with the provided name could not be found.
    ThumbnailNotFound(String),

    /// A media with the provided name could not be found.
    MediaNotFound(String),

    /// Missing luminous intensity on a [Measurement](crate::physical_descriptions::Measurement)
    /// in an [Emitter](crate::physical_descriptions::Emitter).
    MissingLuminousIntensity,

    /// Missing transmission on a [Measurement](crate::physical_descriptions::Measurement)
    /// in a [Filter](crate::physical_descriptions::Filter).
    MissingTransmission,

    /// Polygon had less than three points.
    EmptyPolygon,

    /// Profile had no data points.
    EmptyProfile,

    /// Profile had a duplicate point.
    DuplicatePoint(f64),

    /// CRI group had a duplicate sample.
    DuplicateCriSample(Ces),
}

impl Display for ValidationErrorType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ValidationErrorType::InvalidVersion(version) => {
                write!(f, "invalid version `{version}`")
            }
            ValidationErrorType::MissingName => write!(f, "missing name"),
            ValidationErrorType::DuplicateName => write!(f, "duplicate name"),
            ValidationErrorType::DuplicateSubPhysicalUnit(ty) => {
                write!(f, "duplicate sub physical unit `{ty}`")
            }
            ValidationErrorType::LinkNotFound(obj, node) => write!(f, "no {obj} found at `{node}`"),
            ValidationErrorType::ThumbnailNotFound(thumbnail) => {
                write!(f, "no resource files for thumbnail `{thumbnail}`")
            }
            ValidationErrorType::MediaNotFound(media) => {
                write!(f, "no resource files for media `{media}`")
            }
            ValidationErrorType::MissingLuminousIntensity => {
                write!(f, "missing luminous intensity in measurement")
            }
            ValidationErrorType::MissingTransmission => {
                write!(f, "missing transmission in measurement")
            }
            ValidationErrorType::EmptyPolygon => write!(f, "empty polygon"),
            ValidationErrorType::EmptyProfile => write!(f, "no points"),
            ValidationErrorType::DuplicatePoint(point) => write!(f, "duplicate point `{point}`"),
            ValidationErrorType::DuplicateCriSample(ces) => write!(f, "duplicate sample `{ces}`"),
        }
    }
}

/// Kinds of objects that can raise validation errors.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValidationObject {
    /// [Description](crate::Description)
    Description,

    /// [FixtureType](crate::fixture_type::FixtureType)
    FixtureType,

    /// [ActivationGroup](crate::attribute::ActivationGroup)
    ActivationGroup,

    /// [FeatureGroup](crate::attribute::FeatureGroup)
    FeatureGroup,

    /// [Feature](crate::attribute::Feature)
    Feature,

    /// [Attribute](crate::attribute::Attribute)
    Attribute,

    /// [SubPhysicalUnit](crate::attribute::SubPhysicalUnit)
    SubPhysicalUnit,

    /// [Wheel](crate::wheel::Wheel)
    Wheel,

    /// [WheelSlot](crate::wheel::WheelSlot)
    WheelSlot,

    /// [Emitter](crate::physical_descriptions::Emitter)
    Emitter,

    /// [Filter](crate::physical_descriptions::Filter)
    Filter,

    /// [ColorSpace](crate::physical_descriptions::ColorSpace)
    ColorSpace,

    /// [Gamut](crate::physical_descriptions::Gamut)
    Gamut,

    /// [DmxProfile](crate::physical_descriptions::DmxProfile)
    DmxProfile,

    /// [CriGroup](crate::physical_descriptions::CriGroup)
    CriGroup,

    /// [Connector](crate::physical_descriptions::Connector)
    Connector,

    /// [Model](crate::model::Model)
    Model,

    /// [Geometry](crate::geometry::Geometry)
    Geometry,

    /// [LaserProtocol](crate::geometry::LaserProtocol)
    LaserProtocol,

    /// [PinPatch](crate::geometry::PinPatch)
    PinPatch,

    /// [DmxMode](crate::dmx_mode::DmxMode)
    DmxMode,

    /// [DmxChannel](crate::dmx_mode::DmxChannel)
    DmxChannel,

    /// [LogicalChannel](crate::dmx_mode::LogicalChannel)
    LogicalChannel,

    /// [ChannelFunction](crate::dmx_mode::ChannelFunction)
    ChannelFunction,

    /// [ModeMasterNode](crate::dmx_mode::ModeMasterNode)
    ModeMaster,

    /// [ChannelSet](crate::dmx_mode::ChannelSet)
    ChannelSet,

    /// [SubChannelSet](crate::dmx_mode::SubChannelSet)
    SubChannelSet,

    /// [Relation](crate::dmx_mode::Relation)
    Relation,

    /// [FtMacro](crate::dmx_mode::FtMacro)
    FtMacro,

    /// [SoftwareVersion](crate::protocol::SoftwareVersion)
    SoftwareVersion,

    /// [DmxPersonality](crate::protocol::DmxPersonality)
    DmxPersonality,
}

impl Display for ValidationObject {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ValidationObject::Description => write!(f, "gdtf"),
            ValidationObject::FixtureType => write!(f, "fixture type"),
            ValidationObject::ActivationGroup => write!(f, "activation group"),
            ValidationObject::FeatureGroup => write!(f, "feature group"),
            ValidationObject::Feature => write!(f, "feature"),
            ValidationObject::Attribute => write!(f, "attribute"),
            ValidationObject::SubPhysicalUnit => write!(f, "sub physical unit"),
            ValidationObject::Wheel => write!(f, "wheel"),
            ValidationObject::WheelSlot => write!(f, "wheel slot"),
            ValidationObject::Emitter => write!(f, "emitter"),
            ValidationObject::Filter => write!(f, "filter"),
            ValidationObject::ColorSpace => write!(f, "color space"),
            ValidationObject::Gamut => write!(f, "gamut"),
            ValidationObject::DmxProfile => write!(f, "dmx profile"),
            ValidationObject::CriGroup => write!(f, "cri group"),
            ValidationObject::Connector => write!(f, "connector"),
            ValidationObject::Model => write!(f, "model"),
            ValidationObject::Geometry => write!(f, "geometry"),
            ValidationObject::LaserProtocol => write!(f, "laser protocol"),
            ValidationObject::PinPatch => write!(f, "pin patch"),
            ValidationObject::DmxMode => write!(f, "dmx mode"),
            ValidationObject::DmxChannel => write!(f, "dmx channel"),
            ValidationObject::LogicalChannel => write!(f, "logical channel"),
            ValidationObject::ChannelFunction => write!(f, "channel function"),
            ValidationObject::ModeMaster => write!(f, "mode master"),
            ValidationObject::ChannelSet => write!(f, "channel set"),
            ValidationObject::SubChannelSet => write!(f, "sub channel set"),
            ValidationObject::Relation => write!(f, "relation"),
            ValidationObject::FtMacro => write!(f, "fixture macro"),
            ValidationObject::SoftwareVersion => write!(f, "software version"),
            ValidationObject::DmxPersonality => write!(f, "dmx personality"),
        }
    }
}
