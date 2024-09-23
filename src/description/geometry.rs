//! Physical descriptions of the individual parts of a device.

use crate::description::util::IterUtil;
use crate::fixture_type::FixtureType;
use crate::model::Model;
use crate::physical_descriptions::Emitter;
use crate::validation::{ValidationError, ValidationErrorType, ValidationObject, ValidationResult};
use crate::values::{DmxAddress, Matrix, Name, Node, NodeExt, Vector3};
use serde::{Deserialize, Serialize};

/// Defines methods common to all [Geometry] types.
pub trait AnyGeometry {
    /// The unique name of the geometry.
    fn name(&self) -> Option<&Name>;

    /// Name of the [Model] used by the geometry.
    fn model_name(&self) -> Option<&Name>;

    /// Children of the geometry.
    fn children(&self) -> &[Geometry];

    /// Looks up the [Model] linked by this geometry.
    fn model<'s>(&self, parent_fixture_type: &'s FixtureType) -> Option<&'s Model> {
        parent_fixture_type.model(self.model_name()?)
    }

    /// Looks up a direct child [Geometry] by [name](Geometry::name).
    fn child(&self, name: &str) -> Option<&Geometry> {
        self.children()
            .iter()
            .find(|child| child.name().map(Name::as_ref) == Some(name))
    }

    /// Looks up a nested child [Geometry] by [name](Geometry::name).
    fn nested_child(&self, name: &str) -> Option<&Geometry> {
        for child in self.children() {
            if child.name().map(Name::as_ref) == Some(name) {
                return Some(child);
            };
            if let Some(nested_child) = child.nested_child(name) {
                return Some(nested_child);
            }
        }
        None
    }

    /// Looks up a child [Geometry] by [name](Geometry::name) from a path.
    fn child_node(&self, node: &[Name]) -> Option<&Geometry> {
        let (name, tail) = node.split_first()?;
        let geometry = self.child(name)?;

        if tail.is_empty() {
            Some(geometry)
        } else {
            geometry.child_node(tail)
        }
    }

    /// To be overridden by an implementor: implement custom validation checks in addition to those
    /// performed by [validate](Self::validate).
    #[doc(hidden)]
    fn validate_custom(&self, _parent_fixture_type: &FixtureType, _result: &mut ValidationResult) {}

    /// Performs validation checks on the object.
    fn validate(&self, parent_fixture_type: &FixtureType, result: &mut ValidationResult) {
        if self.name().is_none() {
            result.errors.push(ValidationError::new(
                ValidationObject::Geometry,
                None,
                ValidationErrorType::MissingName,
            ));
        }

        if let (Some(model_name), None) = (self.model_name(), self.model(parent_fixture_type)) {
            result.errors.push(ValidationError::new(
                ValidationObject::Geometry,
                self.name().map(Name::to_string),
                ValidationErrorType::LinkNotFound(
                    ValidationObject::Model,
                    Node::new([model_name.clone()]),
                ),
            ));
        }

        self.validate_custom(parent_fixture_type, result);

        for child in self.children() {
            child.validate(parent_fixture_type, result);
        }
    }
}

/// Defines the physical description of a device's parts.
///
/// Geometry is organised in a tree with each geometry having zero or more child geometries.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Geometry {
    /// Basic geometry without specification.
    #[serde(rename = "Geometry")]
    Generic(GenericGeometry),

    /// Defines device parts with a rotation axis.
    Axis(AxisGeometry),

    /// Defines device parts with a beam filter.
    FilterBeam(BeamFilterGeometry),

    /// Defines device parts with a color filter.
    FilterColor(ColorFilterGeometry),

    /// Defines device parts with gobo wheels.
    FilterGobo(GoboFilterGeometry),

    /// Defines device parts with a shaper.
    FilterShaper(ShaperFilterGeometry),

    /// Defines device parts with a light source.
    Beam(BeamGeometry),

    /// Defines device parts with a layer of a media device that is used for displaying media files.
    MediaServerLayer(MediaServerLayerGeometry),

    /// Defines device parts with a camera or output of a media device.
    MediaServerCamera(MediaServerCameraGeometry),

    /// Defines device parts with a master control of one or several media devices.
    MediaServerMaster(MediaServerMasterGeometry),

    /// Defines device parts with a self-emitting surface used to display visual media.
    Display(DisplayGeometry),

    /// Defines an instance of the same geometry.
    #[serde(rename = "GeometryReference")]
    Reference(ReferenceGeometry),

    /// Defines device parts with a laser's light output.
    Laser(LaserGeometry),

    /// Defines device parts with an electrical device that can be wired.
    WiringObject(WiringObjectGeometry),

    /// Defines an inventory of device parts.
    Inventory(InventoryGeometry),

    /// Defines device parts with a structure.
    Structure(StructureGeometry),

    /// Defines device parts with a support.
    Support(SupportGeometry),

    /// Defines device parts with a magnet.
    Magnet(MagnetGeometry),
}

impl AnyGeometry for Geometry {
    fn name(&self) -> Option<&Name> {
        match self {
            Geometry::Generic(g) => g.name(),
            Geometry::Axis(g) => g.name(),
            Geometry::FilterBeam(g) => g.name(),
            Geometry::FilterColor(g) => g.name(),
            Geometry::FilterGobo(g) => g.name(),
            Geometry::FilterShaper(g) => g.name(),
            Geometry::Beam(g) => g.name(),
            Geometry::MediaServerLayer(g) => g.name(),
            Geometry::MediaServerCamera(g) => g.name(),
            Geometry::MediaServerMaster(g) => g.name(),
            Geometry::Display(g) => g.name(),
            Geometry::Reference(g) => g.name(),
            Geometry::Laser(g) => g.name(),
            Geometry::WiringObject(g) => g.name(),
            Geometry::Inventory(g) => g.name(),
            Geometry::Structure(g) => g.name(),
            Geometry::Support(g) => g.name(),
            Geometry::Magnet(g) => g.name(),
        }
    }

    fn model_name(&self) -> Option<&Name> {
        match self {
            Geometry::Generic(g) => g.model_name(),
            Geometry::Axis(g) => g.model_name(),
            Geometry::FilterBeam(g) => g.model_name(),
            Geometry::FilterColor(g) => g.model_name(),
            Geometry::FilterGobo(g) => g.model_name(),
            Geometry::FilterShaper(g) => g.model_name(),
            Geometry::Beam(g) => g.model_name(),
            Geometry::MediaServerLayer(g) => g.model_name(),
            Geometry::MediaServerCamera(g) => g.model_name(),
            Geometry::MediaServerMaster(g) => g.model_name(),
            Geometry::Display(g) => g.model_name(),
            Geometry::Reference(g) => g.model_name(),
            Geometry::Laser(g) => g.model_name(),
            Geometry::WiringObject(g) => g.model_name(),
            Geometry::Inventory(g) => g.model_name(),
            Geometry::Structure(g) => g.model_name(),
            Geometry::Support(g) => g.model_name(),
            Geometry::Magnet(g) => g.model_name(),
        }
    }

    fn children(&self) -> &[Geometry] {
        match self {
            Geometry::Generic(g) => g.children(),
            Geometry::Axis(g) => g.children(),
            Geometry::FilterBeam(g) => g.children(),
            Geometry::FilterColor(g) => g.children(),
            Geometry::FilterGobo(g) => g.children(),
            Geometry::FilterShaper(g) => g.children(),
            Geometry::Beam(g) => g.children(),
            Geometry::MediaServerLayer(g) => g.children(),
            Geometry::MediaServerCamera(g) => g.children(),
            Geometry::MediaServerMaster(g) => g.children(),
            Geometry::Display(g) => g.children(),
            Geometry::Reference(g) => g.children(),
            Geometry::Laser(g) => g.children(),
            Geometry::WiringObject(g) => g.children(),
            Geometry::Inventory(g) => g.children(),
            Geometry::Structure(g) => g.children(),
            Geometry::Support(g) => g.children(),
            Geometry::Magnet(g) => g.children(),
        }
    }
}

macro_rules! impl_any_geometry {
    ($str:ident) => {
        impl AnyGeometry for $str {
            fn name(&self) -> Option<&Name> {
                self.name.as_ref()
            }
            fn model_name(&self) -> Option<&Name> {
                self.model.as_ref()
            }
            fn children(&self) -> &[Geometry] {
                &self.children
            }
        }
    };
}

/// Basic geometry without specification.
///
/// Corresponds to a `<Geometry>` XML node.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GenericGeometry {
    /// The unique name of the geometry.
    ///
    /// Recommended name for a conventional is "Body".
    /// Recommended name for a geometry representing the base housing of a moving head is "Base".
    ///
    /// Corresponds to the `Name` XML attribute.
    #[serde(rename = "@Name", skip_serializing_if = "Option::is_none")]
    pub name: Option<Name>,

    /// Link to the corresponding model.
    ///
    /// Corresponds to the `Model` XML attribute.
    #[serde(rename = "@Model", skip_serializing_if = "Option::is_none")]
    pub model: Option<Name>,

    /// Relative position of the geometry.
    ///
    /// Corresponds to the `Position` XML attribute.
    #[serde(rename = "@Position")]
    pub position: Matrix,

    /// Children of the geometry.
    ///
    /// Corresponds to all [Geometry] XML child nodes.
    #[serde(rename = "$value", skip_serializing_if = "Vec::is_empty", default)]
    pub children: Vec<Geometry>,
}

impl_any_geometry!(GenericGeometry);

/// Defines device parts with a rotation axis.
///
/// Corresponds to an `<Axis>` XML node.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AxisGeometry {
    /// The unique name of the geometry.
    ///
    /// Recommended name for an axis-geometry is "Yoke".
    /// Recommended name for an axis-geometry representing the lamp housing of a moving head is
    /// "Head".
    /// Note: The Head of a moving head is usually mounted to the Yoke.
    ///
    /// Corresponds to the `Name` XML attribute.
    #[serde(rename = "@Name", skip_serializing_if = "Option::is_none")]
    pub name: Option<Name>,

    /// Link to the corresponding model.
    ///
    /// Corresponds to the `Model` XML attribute.
    #[serde(rename = "@Model", skip_serializing_if = "Option::is_none")]
    pub model: Option<Name>,

    /// Relative position of the geometry.
    ///
    /// Corresponds to the `Position` XML attribute.
    #[serde(rename = "@Position")]
    pub position: Matrix,

    /// Children of the geometry.
    ///
    /// Corresponds to all [Geometry] XML child nodes.
    #[serde(rename = "$value", skip_serializing_if = "Vec::is_empty", default)]
    pub children: Vec<Geometry>,
}

impl_any_geometry!(AxisGeometry);

/// Defines device parts with a beam filter.
///
/// Corresponds to a `<FilterBeam>` XML node.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BeamFilterGeometry {
    /// The unique name of the geometry.
    ///
    /// Recommended name for a beam filter limiting the diffusion of light is "BarnDoor".
    /// Recommended name for a beam filter adjusting the diameter of the beam is "Iris".
    /// Note: BarnDoor and Iris are usually mounted to a conventional.
    ///
    /// Corresponds to the `Name` XML attribute.
    #[serde(rename = "@Name", skip_serializing_if = "Option::is_none")]
    pub name: Option<Name>,

    /// Link to the corresponding model.
    ///
    /// Corresponds to the `Model` XML attribute.
    #[serde(rename = "@Model", skip_serializing_if = "Option::is_none")]
    pub model: Option<Name>,

    /// Relative position of the geometry.
    ///
    /// Corresponds to the `Position` XML attribute.
    #[serde(rename = "@Position")]
    pub position: Matrix,

    /// Children of the geometry.
    ///
    /// Corresponds to all [Geometry] XML child nodes.
    #[serde(rename = "$value", skip_serializing_if = "Vec::is_empty", default)]
    pub children: Vec<Geometry>,
}

impl_any_geometry!(BeamFilterGeometry);

/// Defines device parts with a color filter.
///
/// Corresponds to a `<FilterColor>` XML node.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ColorFilterGeometry {
    /// The unique name of the geometry.
    ///
    /// Recommended name for a filter of a color or a mechanical color changer is "FilterColor".
    /// Note: FilterColor is usually mounted to a conventional.
    ///
    /// Corresponds to the `Name` XML attribute.
    #[serde(rename = "@Name", skip_serializing_if = "Option::is_none")]
    pub name: Option<Name>,

    /// Link to the corresponding model.
    ///
    /// Corresponds to the `Model` XML attribute.
    #[serde(rename = "@Model", skip_serializing_if = "Option::is_none")]
    pub model: Option<Name>,

    /// Relative position of the geometry.
    ///
    /// Corresponds to the `Position` XML attribute.
    #[serde(rename = "@Position")]
    pub position: Matrix,

    /// Children of the geometry.
    ///
    /// Corresponds to all [Geometry] XML child nodes.
    #[serde(rename = "$value", skip_serializing_if = "Vec::is_empty", default)]
    pub children: Vec<Geometry>,
}

impl_any_geometry!(ColorFilterGeometry);

/// Defines device parts with gobo wheels.
///
/// Corresponds to a `<FilterGobo>` XML node.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GoboFilterGeometry {
    /// The unique name of the geometry.
    ///
    /// Recommended name for a filter of a gobo or mechanical gobo changer is "FilterGobo".
    /// Note: FilterGobo is uaually mounted to a conventional.
    ///
    /// Corresponds to the `Name` XML attribute.
    #[serde(rename = "@Name", skip_serializing_if = "Option::is_none")]
    pub name: Option<Name>,

    /// Link to the corresponding model.
    ///
    /// Corresponds to the `Model` XML attribute.
    #[serde(rename = "@Model", skip_serializing_if = "Option::is_none")]
    pub model: Option<Name>,

    /// Relative position of the geometry.
    ///
    /// Corresponds to the `Position` XML attribute.
    #[serde(rename = "@Position")]
    pub position: Matrix,

    /// Children of the geometry.
    ///
    /// Corresponds to all [Geometry] XML child nodes.
    #[serde(rename = "$value", skip_serializing_if = "Vec::is_empty", default)]
    pub children: Vec<Geometry>,
}

impl_any_geometry!(GoboFilterGeometry);

/// Defines device parts with a shaper.
///
/// Corresponds to a `<FilterShaper>` XML node.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ShaperFilterGeometry {
    /// The unique name of the geometry.
    ///
    /// Recommended name for a filter used to form the beam to a framed, triangular or
    /// trapezoid shape is "Shaper".
    /// Note: Shaper is usually mounted to a conventional.
    ///
    /// Corresponds to the `Name` XML attribute.
    #[serde(rename = "@Name", skip_serializing_if = "Option::is_none")]
    pub name: Option<Name>,

    /// Link to the corresponding model.
    ///
    /// Corresponds to the `Model` XML attribute.
    #[serde(rename = "@Model", skip_serializing_if = "Option::is_none")]
    pub model: Option<Name>,

    /// Relative position of the geometry.
    ///
    /// Corresponds to the `Position` XML attribute.
    #[serde(rename = "@Position")]
    pub position: Matrix,

    /// Children of the geometry.
    ///
    /// Corresponds to all [Geometry] XML child nodes.
    #[serde(rename = "$value", skip_serializing_if = "Vec::is_empty", default)]
    pub children: Vec<Geometry>,
}

impl_any_geometry!(ShaperFilterGeometry);

/// Defines device parts with a light source.
///
/// Beam geometry describes the position of the fixture's light output (usually the position of the
/// lens) and not the position of the light source inside the device.
///
/// Corresponds to a `<Beam>` XML node.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BeamGeometry {
    /// The unique name of the geometry.
    ///
    /// Recommended name for a light source of a conventional or moving head or a projector is
    /// "Beam".
    /// Note 1: Beam is usually mounted to Head or Body.
    ///
    /// Recommended name for a self-emitting single light source is "Pixel".
    /// Note 2: Pixel is usually mounted to Head or Body.
    ///
    /// Recommended name for a number of Pixels that are controlled at the same time is "Array".
    /// Note 3: Array is usually mounted to Head or Body.
    ///
    /// Recommended name for a light source of a moving mirror is "Mirror".
    /// Note 4: Mirror is usually mounted to Yoke.
    ///
    /// Corresponds to the `Name` XML attribute.
    #[serde(rename = "@Name", skip_serializing_if = "Option::is_none")]
    pub name: Option<Name>,

    /// Link to the corresponding model.
    ///
    /// Corresponds to the `Model` XML attribute.
    #[serde(rename = "@Model", skip_serializing_if = "Option::is_none")]
    pub model: Option<Name>,

    /// Relative position of the geometry.
    ///
    /// Corresponds to the `Position` XML attribute.
    #[serde(rename = "@Position")]
    pub position: Matrix,

    /// Children of the geometry.
    ///
    /// Corresponds to all [Geometry] XML child nodes.
    #[serde(rename = "$value", skip_serializing_if = "Vec::is_empty", default)]
    pub children: Vec<Geometry>,

    /// Defines the type of light source.
    ///
    /// Corresponds to the `LampType` XML attribute.
    #[serde(rename = "@LampType", default)]
    pub lamp_type: LampType,

    /// Power consumption in watts.
    ///
    /// Corresponds to the `PowerConsumption` XML attribute.
    #[serde(rename = "@PowerConsumption", default = "default_power_consumption")]
    pub power_consumption: f32,

    /// Intensity of all the represented light emitters in lumens.
    ///
    /// Corresponds to the `LuminousFlux` XML attribute.
    #[serde(rename = "@LuminousFlux", default = "default_luminous_flux")]
    pub luminous_flux: f32,

    /// Color temperature in kelvin.
    ///
    /// Corresponds to the `ColorTemperature` XML attribute.
    #[serde(rename = "@ColorTemperature", default = "default_color_temperature")]
    pub color_temperature: f32,

    /// Beam angle in degrees.
    ///
    /// Corresponds to the `BeamAngle` XML attribute.
    #[serde(rename = "@BeamAngle", default = "default_beam_angle")]
    pub beam_angle: f32,

    /// Field angle in degrees.
    ///
    /// Corresponds to the `FieldAngle` XML attribute.
    #[serde(rename = "@FieldAngle", default = "default_field_angle")]
    pub field_angle: f32,

    /// Throw ratio of the lens for rectangle beam types.
    ///
    /// Corresponds to the `ThrowRatio` XML attribute.
    #[serde(rename = "@ThrowRatio", default = "default_throw_ratio")]
    pub throw_ratio: f32,

    /// Ratio from width to height of rectangle beam types.
    ///
    /// Corresponds to the `RectangleRatio` XML attribute.
    #[serde(rename = "@RectangleRatio", default = "default_rectangle_ratio")]
    pub rectangle_ratio: f32,

    /// Beam radius on starting point.
    ///
    /// Corresponds to the `BeamRadius` XML attribute.
    #[serde(rename = "@BeamRadius", default = "default_beam_radius")]
    pub beam_radius: f32,

    /// Describes how the beam will be rendered.
    ///
    /// Corresponds to the `BeamType` XML attribute.
    #[serde(rename = "@BeamType", default)]
    pub beam_type: BeamType,

    /// The CRI according to TM-30.
    ///
    /// A quantitative measure of the ability of the light source to show an object color
    /// compared to a daylight reference.
    ///
    /// Corresponds to the `ColorRenderingIndex` XML attribute.
    #[serde(
        rename = "@ColorRenderingIndex",
        default = "default_color_rendering_index"
    )]
    pub color_rendering_index: u8,

    /// Optional link to an emitter defining the white light source of a subtractive color mixing
    /// system.
    ///
    /// The default spectrum is a black-body with the defined color temperature.
    ///
    /// Corresponds to the `EmitterSpectrum` XML attribute.
    #[serde(rename = "@EmitterSpectrum", skip_serializing_if = "Option::is_none")]
    pub emitter_spectrum: Option<Node>,
}

impl AnyGeometry for BeamGeometry {
    fn name(&self) -> Option<&Name> {
        self.name.as_ref()
    }
    fn model_name(&self) -> Option<&Name> {
        self.model.as_ref()
    }
    fn children(&self) -> &[Geometry] {
        &self.children
    }

    fn validate_custom(&self, parent_fixture_type: &FixtureType, result: &mut ValidationResult) {
        if let (Some(emitter_spectrum), None) = (
            &self.emitter_spectrum,
            self.emitter_spectrum(parent_fixture_type),
        ) {
            result.errors.push(ValidationError::new(
                ValidationObject::Geometry,
                self.name().map(Name::to_string),
                ValidationErrorType::LinkNotFound(
                    ValidationObject::Emitter,
                    emitter_spectrum.clone(),
                ),
            ));
        }
    }
}

impl BeamGeometry {
    /// Looks up the optional [Emitter] linked by this geometry.
    pub fn emitter_spectrum<'s>(
        &self,
        parent_fixture_type: &'s FixtureType,
    ) -> Option<&'s Emitter> {
        let name = self.emitter_spectrum.as_ref()?.single()?;
        parent_fixture_type.physical_descriptions.emitter(name)
    }
}

fn default_color_temperature() -> f32 {
    6000.
}
fn default_color_rendering_index() -> u8 {
    100
}

/// Defines the type of a light source represented by a [BeamGeometry].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum LampType {
    /// Tungsten lamp.
    Tungsten,

    /// Halogen lamp.
    Halogen,

    /// LED lamp.
    #[serde(rename = "LED")]
    Led,

    /// Discharge lamp.
    #[default]
    #[serde(other)]
    Discharge,
}

/// Describes how the beam represented by a [BeamGeometry] will be rendered.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum BeamType {
    /// Rendered as a conical beam with hard edges.
    Spot,
    /// No beam will be drawn, only the geometry will emit light itself.
    None,
    /// Rendered as a pyramid-shaped beam with hard edges.
    Rectangle,
    /// Rendered as a conical beam with soft edges and softened field projection.
    #[serde(rename = "PC")]
    Pc,
    /// Rendered as a conical beam with soft edges and softened field projection.
    Fresnel,
    /// No beam will be drawn, only the geometry will emit light itself.
    Glow,

    /// Rendered as a conical beam with soft edges and softened field projection.
    #[default]
    #[serde(other)]
    Wash,
}

/// Defines device parts with a layer of a media device that is used for displaying media files.
///
/// Corresponds to a `<MediaServerLayer>` XML node.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MediaServerLayerGeometry {
    /// The unique name of the geometry.
    ///
    /// Corresponds to the `Name` XML attribute.
    #[serde(rename = "@Name", skip_serializing_if = "Option::is_none")]
    pub name: Option<Name>,

    /// Link to the corresponding model that will be used to display the alignment in media server
    /// space.
    ///
    /// Corresponds to the `Model` XML attribute.
    #[serde(rename = "@Model", skip_serializing_if = "Option::is_none")]
    pub model: Option<Name>,

    /// Relative position of the geometry.
    ///
    /// Corresponds to the `Position` XML attribute.
    #[serde(rename = "@Position")]
    pub position: Matrix,

    /// Children of the geometry.
    ///
    /// Corresponds to all [Geometry] XML child nodes.
    #[serde(rename = "$value", skip_serializing_if = "Vec::is_empty", default)]
    pub children: Vec<Geometry>,
}

impl_any_geometry!(MediaServerLayerGeometry);

/// Defines device parts with a camera or output of a media device.
///
///
/// Corresponds to a `<MediaServerCamera>` XML node.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MediaServerCameraGeometry {
    /// The unique name of the geometry.
    ///
    /// Corresponds to the `Name` XML attribute.
    #[serde(rename = "@Name", skip_serializing_if = "Option::is_none")]
    pub name: Option<Name>,

    /// Link to the corresponding model that will be used to display the alignment in media server
    /// space.
    ///
    /// Corresponds to the `Model` XML attribute.
    #[serde(rename = "@Model", skip_serializing_if = "Option::is_none")]
    pub model: Option<Name>,

    /// Relative position of the geometry.
    ///
    /// Corresponds to the `Position` XML attribute.
    #[serde(rename = "@Position")]
    pub position: Matrix,

    /// Children of the geometry.
    ///
    /// Corresponds to all [Geometry] XML child nodes.
    #[serde(rename = "$value", skip_serializing_if = "Vec::is_empty", default)]
    pub children: Vec<Geometry>,
}

impl_any_geometry!(MediaServerCameraGeometry);

/// Defines device parts with a master control of one or several media devices.
///
/// Corresponds to a `<MediaServerMaster>` XML node.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MediaServerMasterGeometry {
    /// The unique name of the geometry.
    ///
    /// Corresponds to the `Name` XML attribute.
    #[serde(rename = "@Name", skip_serializing_if = "Option::is_none")]
    pub name: Option<Name>,

    /// Link to the corresponding model.
    ///
    /// Corresponds to the `Model` XML attribute.
    #[serde(rename = "@Model", skip_serializing_if = "Option::is_none")]
    pub model: Option<Name>,

    /// Relative position of the geometry.
    ///
    /// Corresponds to the `Position` XML attribute.
    #[serde(rename = "@Position")]
    pub position: Matrix,

    /// Children of the geometry.
    ///
    /// Corresponds to all [Geometry] XML child nodes.
    #[serde(rename = "$value", skip_serializing_if = "Vec::is_empty", default)]
    pub children: Vec<Geometry>,
}

impl_any_geometry!(MediaServerMasterGeometry);

/// Defines device parts with a self-emitting surface used to display visual media.
///
/// Corresponds to a `<Display>` XML node.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DisplayGeometry {
    /// The unique name of the geometry.
    ///
    /// Corresponds to the `Name` XML attribute.
    #[serde(rename = "@Name", skip_serializing_if = "Option::is_none")]
    pub name: Option<Name>,

    /// Link to the corresponding model.
    ///
    /// Corresponds to the `Model` XML attribute.
    #[serde(rename = "@Model", skip_serializing_if = "Option::is_none")]
    pub model: Option<Name>,

    /// Relative position of the geometry.
    ///
    /// Corresponds to the `Position` XML attribute.
    #[serde(rename = "@Position")]
    pub position: Matrix,

    /// Children of the geometry.
    ///
    /// Corresponds to all [Geometry] XML child nodes.
    #[serde(rename = "$value", skip_serializing_if = "Vec::is_empty", default)]
    pub children: Vec<Geometry>,

    /// Name of the mapped texture in the model file that will be swapped out for the media
    /// resource.
    ///
    /// Corresponds to the `Texture` XML attribute.
    #[serde(rename = "@Texture")]
    pub texture: String,
}

impl_any_geometry!(DisplayGeometry);

/// Defines an instance of the same geometry.
///
/// For example: an LED panel with multiple pixels.
///
/// Corresponds to a `<GeometryReference>` XML node.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReferenceGeometry {
    /// The unique name of the geometry.
    ///
    /// Corresponds to the `Name` XML attribute.
    #[serde(rename = "@Name", skip_serializing_if = "Option::is_none")]
    pub name: Option<Name>,

    /// An optional link to a corresponding model.
    ///
    /// The model replaces the model of the parent of the referenced geometry. The models of the
    /// children are not affected. If the model is not set, the model is taken from the referenced
    /// geometry.
    ///
    /// Corresponds to the `Model` XML attribute.
    #[serde(rename = "@Model", skip_serializing_if = "Option::is_none")]
    pub model: Option<Name>,

    /// Relative position of the geometry.
    ///
    /// Corresponds to the `Position` XML attribute.
    #[serde(rename = "@Position")]
    pub position: Matrix,

    /// Name of the referenced geometry.
    ///
    /// Only top level geometries can be referenced.
    ///
    /// Corresponds to the `Geometry` XML attribute.
    #[serde(rename = "@Geometry")]
    pub geometry: Option<Name>,

    /// Specifies DMX offsets for the DMX channels of the referenced geometry.
    ///
    /// The number of breaks here depends on the number of different breaks in the DMX channels of
    /// the referenced geometry. For example, if the referenced geometry has DMX channels with DMX
    /// break 2 and 4, this geometry must have 2 breaks, the first with the DMX offset for DMX break
    /// 2 and the second for DMX break 4.
    ///
    /// If one or more of the DMX channels of the referenced geometry has the special value
    /// "Overwrite" as a DMX break, the DMX break for those channels and the DMX offsets needs to be
    /// defined.
    ///
    /// Corresponds to all `<Break>` XML child nodes.
    #[serde(rename = "Break", skip_serializing_if = "Vec::is_empty", default)]
    pub breaks: Vec<Break>,
}

impl AnyGeometry for ReferenceGeometry {
    fn name(&self) -> Option<&Name> {
        self.name.as_ref()
    }

    fn model_name(&self) -> Option<&Name> {
        self.model.as_ref()
    }

    fn children(&self) -> &[Geometry] {
        &[]
    }

    fn validate_custom(&self, parent_fixture_type: &FixtureType, result: &mut ValidationResult) {
        if let (Some(geometry), None) = (&self.geometry, self.geometry(parent_fixture_type)) {
            result.errors.push(ValidationError::new(
                ValidationObject::Geometry,
                self.name().map(Name::to_string),
                ValidationErrorType::LinkNotFound(
                    ValidationObject::Geometry,
                    Node::new([geometry.clone()]),
                ),
            ));
        }
    }
}

impl ReferenceGeometry {
    /// Looks up the [Geometry] linked by this geometry.
    pub fn geometry<'s>(&self, parent_fixture_type: &'s FixtureType) -> Option<&'s Geometry> {
        parent_fixture_type.root_geometry(self.geometry.as_ref()?)
    }
}

/// Specifies the DMX offset for the DMX channel of the referenced geometry.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Break {
    /// DMX offset.
    ///
    /// A value of 1 means no offset for the corresponding DMX channel.
    ///
    /// Corresponds to the `DMXOffset` XML attribute.
    #[serde(rename = "@DMXOffset", default = "default_break_dmx_offset")]
    pub dmx_offset: DmxAddress,

    /// Defines the unique number of the DMX break for which the offset is given.
    ///
    /// Corresponds to the `DMXBreak` XML attribute.
    #[serde(rename = "@DMXBreak", default = "default_break_dmx_break")]
    pub dmx_break: u8,
}

fn default_break_dmx_offset() -> DmxAddress {
    DmxAddress::from_absolute(1)
}
fn default_break_dmx_break() -> u8 {
    1
}

/// Defines device parts with a laser's light output.
///
/// Corresponds to a `<Laser>` XML node.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LaserGeometry {
    /// The unique name of the geometry.
    ///
    /// Corresponds to the `Name` XML attribute.
    #[serde(rename = "@Name", skip_serializing_if = "Option::is_none")]
    pub name: Option<Name>,

    /// Link to the corresponding model.
    ///
    /// Corresponds to the `Model` XML attribute.
    #[serde(rename = "@Model", skip_serializing_if = "Option::is_none")]
    pub model: Option<Name>,

    /// Relative position of the geometry.
    ///
    /// Corresponds to the `Position` XML attribute.
    #[serde(rename = "@Position")]
    pub position: Matrix,

    /// Children of the geometry.
    ///
    /// Corresponds to all [Geometry] XML child nodes.
    #[serde(rename = "$value", skip_serializing_if = "Vec::is_empty", default)]
    pub children: Vec<Geometry>,

    /// Type of color emitted by the laser.
    ///
    /// Corresponds to the `ColorType` XML attribute.
    #[serde(flatten, default)]
    pub color_type: ColorType,

    /// Output strength of the laser in watts.
    ///
    /// Corresponds to the `OutputStrength` XML attribute.
    #[serde(rename = "@OutputStrength")]
    pub output_strength: f32,

    /// Optional link to the emitter group.
    ///
    /// Corresponds to the `Emitter` XML attribute.
    #[serde(rename = "@Emitter", skip_serializing_if = "Option::is_none")]
    pub emitter: Option<Node>,

    /// Beam diameter where it leaves the projector in meters.
    ///
    /// Corresponds to the `BeamDiameter` XML attribute.
    #[serde(rename = "@BeamDiameter")]
    pub beam_diameter: f32,

    /// Minimum beam divergence in milliradians.
    ///
    /// Corresponds to the `BeamDivergenceMin` XML attribute.
    #[serde(rename = "@BeamDivergenceMin")]
    pub beam_divergence_min: f32,

    /// Maximum beam divergence in milliradians.
    ///
    /// Corresponds to the `BeamDivergenceMax` XML attribute.
    #[serde(rename = "@BeamDivergenceMax")]
    pub beam_divergence_max: f32,

    /// Possible total scan angle pan of the beam in degrees.
    ///
    /// Corresponds to the `ScanAnglePan` XML attribute.
    #[serde(rename = "@ScanAnglePan")]
    pub scan_angle_pan: f32,

    /// Possible total scan angle tilt of the beam in degrees.
    ///
    /// Corresponds to the `ScanAngleTilt` XML attribute.
    #[serde(rename = "@ScanAngleTilt")]
    pub scan_angle_tilt: f32,

    /// Speed of the beam in kilo points per second.
    ///
    /// Corresponds to the `ScanSpeed` XML attribute.
    #[serde(rename = "@ScanSpeed")]
    pub scan_speed: f32,

    /// Supported protocols of the laser.
    ///
    /// Corresponds to all `<Protocol>` XML child nodes.
    #[serde(rename = "Protocol", skip_serializing_if = "Vec::is_empty", default)]
    pub protocols: Vec<LaserProtocol>,
}

impl AnyGeometry for LaserGeometry {
    fn name(&self) -> Option<&Name> {
        self.name.as_ref()
    }
    fn model_name(&self) -> Option<&Name> {
        self.model.as_ref()
    }
    fn children(&self) -> &[Geometry] {
        &self.children
    }

    fn validate_custom(&self, parent_fixture_type: &FixtureType, result: &mut ValidationResult) {
        if let (Some(emitter), None) = (&self.emitter, self.emitter(parent_fixture_type)) {
            result.errors.push(ValidationError::new(
                ValidationObject::Geometry,
                self.name().map(Name::to_string),
                ValidationErrorType::LinkNotFound(ValidationObject::Emitter, emitter.clone()),
            ));
        }

        let duplicate_protocol_name = self
            .protocols
            .iter()
            .filter_map(|protocol| protocol.name.as_ref())
            .find_duplicate();
        if let Some(name) = duplicate_protocol_name {
            result.errors.push(ValidationError::new(
                ValidationObject::LaserProtocol,
                name.to_string(),
                ValidationErrorType::DuplicateName,
            ));
        }

        for protocol in &self.protocols {
            protocol.validate(result);
        }
    }
}

impl LaserGeometry {
    /// Looks up the optional [Emitter] linked by this geometry.
    pub fn emitter<'s>(&self, parent_fixture_type: &'s FixtureType) -> Option<&'s Emitter> {
        let name = self.emitter.as_ref()?.single()?;
        parent_fixture_type.physical_descriptions.emitter(name)
    }
}

/// Type of color emitted by a laser represented by a [LaserGeometry].
#[derive(Debug, Clone, Copy, PartialEq, Default, Serialize, Deserialize)]
#[serde(tag = "@ColorType")]
pub enum ColorType {
    /// The laser geometry emits a single wavelength only.
    SingleWaveLength(
        /// Color in nanometers.
        ///
        /// Corresponds to the `Color` XML attribute.
        #[serde(rename = "@Color")]
        f32,
    ),

    /// The laser geometry emits an additive RGB color.
    #[default]
    #[serde(other, rename = "RGB")]
    Rgb,
}

/// Defines device parts with an electrical device that can be wired.
///
/// Corresponds to a `<WiringObject>` XML node.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WiringObjectGeometry {
    /// The unique name of the geometry.
    ///
    /// This name is also the name of the interface to the outside.
    ///
    /// Corresponds to the `Name` XML attribute.
    #[serde(rename = "@Name", skip_serializing_if = "Option::is_none")]
    pub name: Option<Name>,

    /// Link to the corresponding model.
    ///
    /// Corresponds to the `Model` XML attribute.
    #[serde(rename = "@Model", skip_serializing_if = "Option::is_none")]
    pub model: Option<Name>,

    /// Relative position of the geometry.
    ///
    /// Corresponds to the `Position` XML attribute.
    #[serde(rename = "@Position")]
    pub position: Matrix,

    /// Children of the geometry.
    ///
    /// Corresponds to all [Geometry] XML child nodes.
    #[serde(rename = "$value", skip_serializing_if = "Vec::is_empty", default)]
    pub children: Vec<Geometry>,

    /// The type of the connector.
    ///
    /// A list of predefined types is defined in [Annex D](https://gdtf.eu/gdtf/annex/annex-d/) of
    /// the GDTF specification. Custom types of connector can also be defined, for example
    /// "Loose End".
    ///
    /// Corresponds to the `ConnectorType` XML attribute.
    #[serde(rename = "@ConnectorType")]
    pub connector_type: String,

    /// The type of the electrical component used.
    ///
    /// Corresponds to the `ComponentType` XML attribute.
    #[serde(flatten)]
    pub component_type: ComponentType,

    /// The type of the signal used.
    ///
    /// Predefined values are "Power", "DMX512", "Protocol", "AES", "AnalogVideo", "AnalogAudio".
    /// Custom protocols can also be used here.
    ///
    /// Corresponds to the `SignalType` XML attribute.
    #[serde(rename = "@SignalType")]
    pub signal_type: String,

    /// The number of available pins of the connector type to connect internal wiring to it.
    ///
    /// Corresponds to the `PinCount` XML attribute.
    #[serde(rename = "@PinCount")]
    pub pin_count: i32,

    /// The layer of the signal type.
    ///
    /// In one device, all wiring geometry that uses the same signal layer is connected.
    ///
    /// The special value of "0" indicates the wiring geometry is connected to all geometries.
    ///
    /// Corresponds to the `SignalLayer` XML attribute.
    #[serde(rename = "@SignalLayer")]
    pub signal_layer: i32,

    /// Where the pins are placed on the object.
    ///
    /// Corresponds to the `Orientation` XML attribute.
    #[serde(rename = "@Orientation")]
    pub orientation: Orientation,

    /// Name of the group to which this wiring object belongs.
    ///
    /// Corresponds to the `WireGroup` XML attribute.
    #[serde(rename = "@WireGroup")]
    pub wire_group: String,

    /// A list of patches specifying how different sockets are connected to the pins of other
    /// wiring objects.
    ///
    /// Corresponds to all `<PinPatch>` XML child nodes.
    #[serde(rename = "PinPatch", skip_serializing_if = "Vec::is_empty", default)]
    pub pin_patches: Vec<PinPatch>,
}

impl AnyGeometry for WiringObjectGeometry {
    fn name(&self) -> Option<&Name> {
        self.name.as_ref()
    }
    fn model_name(&self) -> Option<&Name> {
        self.model.as_ref()
    }
    fn children(&self) -> &[Geometry] {
        &self.children
    }

    fn validate_custom(&self, parent_fixture_type: &FixtureType, result: &mut ValidationResult) {
        for pin_patch in &self.pin_patches {
            pin_patch.validate(parent_fixture_type, result);
        }
    }
}

/// The type of electrical component used in a wiring object represented by a
/// [WiringObjectGeometry].
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
#[serde(tag = "@ComponentType")]
pub enum ComponentType {
    /// Input component.
    Input,

    /// Output component.
    Output,

    /// Power source component.
    PowerSource {
        /// The maximum electrical payload that this power source can handle in voltamperes.
        ///
        /// Only for Power Source connector types.
        ///
        /// Corresponds to the `MaxPayLoad` XML attribute.
        #[serde(rename = "@MaxPayLoad", skip_serializing_if = "Option::is_none")]
        max_pay_load: Option<f32>,

        /// The voltage output that this power source can handle in volts.
        ///
        /// Only for Power Source connector types.
        ///
        /// Corresponds to the `Voltage` XML attribute.
        #[serde(rename = "@Voltage")]
        voltage: Option<f32>,
    },

    /// Consumer component.
    Consumer {
        /// The electrical consumption in watts.
        ///
        /// Only for Consumer connector types.
        ///
        /// Corresponds to the `ElectricalPayLoad` XML attribute.
        #[serde(rename = "@ElectricalPayLoad", skip_serializing_if = "Option::is_none")]
        electrical_pay_load: Option<f32>,

        /// The voltage range's maximum value in volts.
        ///
        /// Only for Consumer connector types.
        ///
        /// Corresponds to the `VoltageRangeMax` XML attribute.
        #[serde(rename = "@VoltageRangeMax", skip_serializing_if = "Option::is_none")]
        voltage_range_max: Option<f32>,

        /// The voltage range's minimum value in volts.
        ///
        /// Only for Consumer connector types.
        ///
        /// Corresponds to the `VoltageRangeMin` XML attribute.
        #[serde(rename = "@VoltageRangeMin", skip_serializing_if = "Option::is_none")]
        voltage_range_min: Option<f32>,

        /// The frequency range's maximum value in hertz.
        ///
        /// Only for Consumer connector types.
        ///
        /// Corresponds to the `FrequencyRangeMax` XML attribute.
        #[serde(rename = "@FrequencyRangeMax", skip_serializing_if = "Option::is_none")]
        frequency_range_max: Option<f32>,

        /// The frequency range's minimum value in hertz.
        ///
        /// Only for Consumer connector types.
        ///
        /// Corresponds to the `FrequencyRangeMin` XML attribute.
        #[serde(rename = "@FrequencyRangeMin", skip_serializing_if = "Option::is_none")]
        frequency_range_min: Option<f32>,

        /// The Power Factor of the device.
        ///
        /// Only for Consumer connector types.
        ///
        /// Corresponds to the `CosPhi` XML attribute.
        #[serde(rename = "@CosPhi", skip_serializing_if = "Option::is_none")]
        cos_phi: Option<f32>,
    },

    /// Fuse component.
    Fuse {
        /// The fuse value in amperes.
        ///
        /// Only for Fuse connector types.
        ///
        /// Corresponds to the `FuseCurrent` XML attribute.
        #[serde(rename = "@FuseCurrent", skip_serializing_if = "Option::is_none")]
        fuse_current: Option<f32>,

        /// Fuse rating.
        ///
        /// Only for Fuse connector types.
        ///
        /// Corresponds to the `FuseRating` XML attribute.
        #[serde(rename = "@FuseRating", skip_serializing_if = "Option::is_none")]
        fuse_rating: Option<FuseRating>,
    },

    /// Network provider component.
    NetworkProvider,

    /// Network input component.
    NetworkInput,

    /// Network output component.
    NetworkOutput,

    /// Network input/output component.
    NetworkInOut,
}

/// Fuse rating for a fuse represented by a [WiringObjectGeometry].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum FuseRating {
    /// Type B fuse.
    B,

    /// Type C fuse.
    C,

    /// Type D fuse.
    D,

    /// Type K fuse.
    K,

    /// Type Z fuse.
    Z,
}

/// Where the pins represented by a [WiringObjectGeometry] are located.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Orientation {
    /// Pins face left.
    Left,

    /// Pins face right.
    Right,

    /// Pins face above.
    Top,

    /// Pins face below.
    Bottom,
}

/// Specifies how the different sockets of a wiring object are connected to the pins of other wiring
/// objects.
///
/// Corresponds to a `<PinPatch>` XML node.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PinPatch {
    /// Link to the wiring object connected through this pin patch.
    ///
    /// Corresponds to the `ToWiringObject` XML attribute.
    #[serde(rename = "@ToWiringObject")]
    to_wiring_object: Node,

    /// The pin number used by the parent wiring object to connect to the target wiring object.
    ///
    /// Corresponds to the `FromPin` XML attribute.
    #[serde(rename = "@FromPin")]
    from_pin: i32,

    /// The pin number used by the targeted wiring object to connect to the parent wiring object.
    ///
    /// Corresponds to the `ToPin` XML attribute.
    #[serde(rename = "@ToPin")]
    to_pin: i32,
}

impl PinPatch {
    /// Looks up the [WiringObjectGeometry] linked by this pin patch.
    pub fn to_wiring_object<'s>(
        &self,
        parent_fixture_type: &'s FixtureType,
    ) -> Option<&'s Geometry> {
        parent_fixture_type.geometry_node(&self.to_wiring_object)
    }

    /// Performs validation checks on the object.
    pub fn validate(&self, parent_fixture_type: &FixtureType, result: &mut ValidationResult) {
        if self.to_wiring_object(parent_fixture_type).is_none() {
            result.errors.push(ValidationError::new(
                ValidationObject::PinPatch,
                None,
                ValidationErrorType::LinkNotFound(
                    ValidationObject::Geometry,
                    self.to_wiring_object.clone(),
                ),
            ));
        }
    }
}

/// Defines an inventory of device parts.
///
/// Corresponds to an `<Inventory>` XML node.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InventoryGeometry {
    /// The unique name of the geometry.
    ///
    /// Corresponds to the `Name` XML attribute.
    #[serde(rename = "@Name", skip_serializing_if = "Option::is_none")]
    pub name: Option<Name>,

    /// Link to the corresponding model.
    ///
    /// Corresponds to the `Model` XML attribute.
    #[serde(rename = "@Model", skip_serializing_if = "Option::is_none")]
    pub model: Option<Name>,

    /// Relative position of the geometry.
    ///
    /// Corresponds to the `Position` XML attribute.
    #[serde(rename = "@Position")]
    pub position: Matrix,

    /// Children of the geometry.
    ///
    /// Corresponds to all [Geometry] XML child nodes.
    #[serde(rename = "$value", skip_serializing_if = "Vec::is_empty", default)]
    pub children: Vec<Geometry>,

    /// The default count for new objects.
    ///
    /// Corresponds to the `Count` XML attribute.
    #[serde(rename = "@Count")]
    pub count: i32,
}

impl_any_geometry!(InventoryGeometry);

/// Defines device parts with a structure.
///
/// Corresponds to a `<Structure>` XML node.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StructureGeometry {
    /// The unique name of the geometry.
    ///
    /// Corresponds to the `Name` XML attribute.
    #[serde(rename = "@Name", skip_serializing_if = "Option::is_none")]
    pub name: Option<Name>,

    /// Link to the corresponding model.
    ///
    /// Corresponds to the `Model` XML attribute.
    #[serde(rename = "@Model", skip_serializing_if = "Option::is_none")]
    pub model: Option<Name>,

    /// Relative position of the geometry.
    ///
    /// Corresponds to the `Position` XML attribute.
    #[serde(rename = "@Position")]
    pub position: Matrix,

    /// Children of the geometry.
    ///
    /// Corresponds to all [Geometry] XML child nodes.
    #[serde(rename = "$value", skip_serializing_if = "Vec::is_empty", default)]
    pub children: Vec<Geometry>,

    /// The linked geometry.
    ///
    /// Corresponds to the `LinkedGeometry` XML attribute.
    #[serde(rename = "@LinkedGeometry")]
    pub linked_geometry: Name,

    /// The type of structure.
    ///
    /// Corresponds to the `StructureType` XML attribute.
    #[serde(rename = "@StructureType")]
    pub structure_type: StructureType,

    /// The type of cross section.
    ///
    /// Corresponds to the `CrossSectionType` XML attribute.
    #[serde(flatten)]
    pub cross_section_type: CrossSectionType,
}

impl_any_geometry!(StructureGeometry);

impl StructureGeometry {
    /// Looks up the [Geometry] linked by this geometry.
    pub fn linked_geometry<'s>(
        &self,
        parent_fixture_type: &'s FixtureType,
    ) -> Option<&'s Geometry> {
        parent_fixture_type.nested_geometry(&self.linked_geometry)
    }
}

/// The type of structure represented by a [StructureGeometry].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum StructureType {
    /// Structure describes a center line.
    CenterLineBased,

    /// Structure describes detail.
    Detail,
}

/// The type of cross section of the structure represented by a [StructureGeometry].
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "@CrossSectionType")]
pub enum CrossSectionType {
    /// Structure has a truss framework cross section.
    TrussFramework {
        /// The name of the truss cross section.
        ///
        /// Corresponds to the `TrussCrossSection` XML attribute.
        #[serde(rename = "@TrussCrossSection")]
        section: String,
    },

    /// Structure has a tube cross section.
    Tube {
        /// The height of the cross section.
        ///
        /// Corresponds to the `CrossSectionHeight` XML attribute.
        #[serde(rename = "@CrossSectionHeight")]
        height: f32,

        /// The thickness of the wall of the cross section.
        ///
        /// Corresponds to the `CrossSectionWallThickness` XML attribute.
        #[serde(rename = "@CrossSectionWallThickness")]
        wall_thickness: f32,
    },
}

/// Defines device parts with a support.
///
/// Corresponds to a `<Support>` XML node.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SupportGeometry {
    /// The unique name of the geometry.
    ///
    /// Corresponds to the `Name` XML attribute.
    #[serde(rename = "@Name", skip_serializing_if = "Option::is_none")]
    pub name: Option<Name>,

    /// Link to the corresponding model.
    ///
    /// Corresponds to the `Model` XML attribute.
    #[serde(rename = "@Model", skip_serializing_if = "Option::is_none")]
    pub model: Option<Name>,

    /// Relative position of the geometry.
    ///
    /// Corresponds to the `Position` XML attribute.
    #[serde(rename = "@Position")]
    pub position: Matrix,

    /// Children of the geometry.
    ///
    /// Corresponds to all [Geometry] XML child nodes.
    #[serde(rename = "$value", skip_serializing_if = "Vec::is_empty", default)]
    pub children: Vec<Geometry>,

    /// The type of support.
    ///
    /// Corresponds to the `SupportType` XML attribute.
    #[serde(flatten)]
    pub support_type: SupportType,

    /// The allowable force on the X-axis applied to the object according to the Eurocode, in N.
    ///
    /// Corresponds to the `CapacityX` XML attribute.
    #[serde(rename = "@CapacityX")]
    pub capacity_x: f32,

    /// The allowable force on the Y-axis applied to the object according to the Eurocode, in N.
    ///
    /// Corresponds to the `CapacityY` XML attribute.
    #[serde(rename = "@CapacityY")]
    pub capacity_y: f32,

    /// The allowable force on the Z-axis applied to the object according to the Eurocode, in N.
    ///
    /// Corresponds to the `CapacityZ` XML attribute.
    #[serde(rename = "@CapacityZ")]
    pub capacity_z: f32,

    /// The allowable moment around the X-axis applied to the object according to the Eurocode, in
    /// N/m.
    ///
    /// Corresponds to the `CapacityXX` XML attribute.
    #[serde(rename = "@CapacityXX")]
    pub capacity_xx: f32,

    /// The allowable moment around the Y-axis applied to the object according to the Eurocode, in
    /// N/m.
    ///
    /// Corresponds to the `CapacityYY` XML attribute.
    #[serde(rename = "@CapacityYY")]
    pub capacity_yy: f32,

    /// The allowable moment around the Z-axis applied to the object according to the Eurocode, in
    /// N/m.
    ///
    /// Corresponds to the `CapacityZZ` XML attribute.
    #[serde(rename = "@CapacityZZ")]
    pub capacity_zz: f32,
}

impl_any_geometry!(SupportGeometry);

/// The type of support represented by a [SupportGeometry].
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "@SupportType")]
pub enum SupportType {
    /// Support is a rope.
    Rope {
        /// The name of the rope cross section.
        ///
        /// Corresponds to the `RopeCrossSection` XML attribute.
        #[serde(rename = "@RopeCrossSection")]
        cross_section: String,

        /// The offset of the rope from bottom to top in meters.
        ///
        /// Corresponds to the `RopeOffset` XML attribute.
        #[serde(rename = "@RopeOffset")]
        offset: Vector3,
    },

    /// Support is on the ground.
    GroundSupport {
        /// The compression ratio for this support along the X-axis in N/m.
        ///
        /// Corresponds to the `ResistanceX` XML attribute.
        #[serde(rename = "@ResistanceX")]
        resistance_x: f32,

        /// The compression ratio for this support along the Y-axis in N/m.
        ///
        /// Corresponds to the `ResistanceY` XML attribute.
        #[serde(rename = "@ResistanceY")]
        resistance_y: f32,

        /// The compression ratio for this support along the Z-axis in N/m.
        ///
        /// Corresponds to the `ResistanceZ` XML attribute.
        #[serde(rename = "@ResistanceZ")]
        resistance_z: f32,

        /// The compression ratio for this support around the X-axis in N/m.
        ///
        /// Corresponds to the `ResistanceXX` XML attribute.
        #[serde(rename = "@ResistanceXX")]
        resistance_xx: f32,

        /// The compression ratio for this support around the Y-axis in N/m.
        ///
        /// Corresponds to the `ResistanceYY` XML attribute.
        #[serde(rename = "@ResistanceYY")]
        resistance_yy: f32,

        /// The compression ratio for this support around the Z-axis in N/m.
        ///
        /// Corresponds to the `ResistanceZZ` XML attribute.
        #[serde(rename = "@ResistanceZZ")]
        resistance_zz: f32,
    },
}

/// Defines device parts with a magnet.
///
/// A magnet is a point where other geometries should be attached.
///
/// Corresponds to a `<Magnet>` XML node.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MagnetGeometry {
    /// The unique name of the geometry.
    ///
    /// Corresponds to the `Name` XML attribute.
    #[serde(rename = "@Name", skip_serializing_if = "Option::is_none")]
    pub name: Option<Name>,

    /// Link to the corresponding model.
    ///
    /// Corresponds to the `Model` XML attribute.
    #[serde(rename = "@Model", skip_serializing_if = "Option::is_none")]
    pub model: Option<Name>,

    /// Relative position of the geometry.
    ///
    /// Corresponds to the `Position` XML attribute.
    #[serde(rename = "@Position")]
    pub position: Matrix,

    /// Children of the geometry.
    ///
    /// Corresponds to all [Geometry] XML child nodes.
    #[serde(rename = "$value", skip_serializing_if = "Vec::is_empty", default)]
    pub children: Vec<Geometry>,
}

impl_any_geometry!(MagnetGeometry);

fn default_power_consumption() -> f32 {
    1000.
}
fn default_luminous_flux() -> f32 {
    10000.
}
fn default_beam_angle() -> f32 {
    25.
}
fn default_field_angle() -> f32 {
    25.
}
fn default_throw_ratio() -> f32 {
    1.
}
fn default_rectangle_ratio() -> f32 {
    1.7777
}
fn default_beam_radius() -> f32 {
    0.05
}

/// Specifies the protocol for a Laser.
///
/// Corresponds to a `<Protocol>` XML node.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LaserProtocol {
    /// Name of the protocol.
    ///
    /// Corresponds to the `Name` XML attribute.
    #[serde(rename = "@Name", skip_serializing_if = "Option::is_none")]
    pub name: Option<Name>,
}

impl LaserProtocol {
    /// Performs validation checks on the object.
    pub fn validate(&self, result: &mut ValidationResult) {
        if self.name.is_none() {
            result.errors.push(ValidationError::new(
                ValidationObject::LaserProtocol,
                None,
                ValidationErrorType::MissingName,
            ));
        }
    }
}
