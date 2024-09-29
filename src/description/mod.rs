use crate::description::util::IterUtil;
use crate::fixture_type::FixtureType;
use crate::validation::{ValidationError, ValidationErrorType, ValidationObject, ValidationResult};
use crate::values::{Name, Version};
use crate::ResourceMap;
use serde::{Deserialize, Serialize};

#[macro_use]
mod collect_helper;
mod util;

pub mod attribute;
pub mod dmx_mode;
pub mod fixture_type;
pub mod ft_preset;
pub mod geometry;
pub mod model;
pub mod physical_descriptions;
pub mod protocol;
pub mod revision;
pub mod values;
pub mod wheel;

/// Description of fixtures in a GDTF file.
///
/// Corresponds to the root-level `<GDTF>` node.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename = "GDTF")]
pub struct Description {
    /// Defines the minimal version of compatability.
    #[serde(rename = "@DataVersion")]
    pub data_version: Version,

    /// All fixture types defined in this file.
    #[serde(rename = "FixtureType", skip_serializing_if = "Vec::is_empty", default)]
    pub fixture_types: Vec<FixtureType>,
}

impl Description {
    /// Looks up a [FixtureType] by [name](FixtureType::name).
    pub fn fixture_type(&self, name: &str) -> Option<&FixtureType> {
        self.fixture_types
            .iter()
            .find(|ty| ty.name.as_ref().map(Name::as_ref) == Some(name))
    }

    /// Performs validation checks on the object.
    pub fn validate(&self, resource_map: &mut ResourceMap, result: &mut ValidationResult) {
        if !self.data_version.is_supported() {
            result.errors.push(ValidationError::new(
                ValidationObject::Description,
                None,
                ValidationErrorType::InvalidVersion(self.data_version),
            ));
        }

        let duplicate_fixture_type_name = self
            .fixture_types
            .iter()
            .filter_map(|fixture_type| fixture_type.name.as_ref())
            .find_duplicate();
        if let Some(name) = duplicate_fixture_type_name {
            result.errors.push(ValidationError::new(
                ValidationObject::FixtureType,
                name.to_string(),
                ValidationErrorType::DuplicateName,
            ));
        }

        for fixture_type in &self.fixture_types {
            fixture_type.validate(resource_map, result);
        }
    }
}
