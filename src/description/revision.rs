//! History of a device type.

use serde::{Deserialize, Serialize};

/// Defines one revision of the device type.
///
/// Revisions are optional. Every time a GDTF file is uploaded to the database, a revision with the
/// actual time and UserID is created by the database.
///
/// Corresponds to a `<Revision>` XML node.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Revision {
    /// User-defined text for this revision.
    ///
    /// Corresponds to the `Text` XML attribute.
    #[serde(rename = "@Text", skip_serializing_if = "str::is_empty", default)]
    pub text: String,

    /// Revision date and time.
    ///
    /// Corresponds to the `Date` XML attribute.
    #[serde(rename = "@Date")]
    pub date: String,

    /// UserID of the user that has uploaded the GDTF file to the database.
    ///
    /// Corresponds to the `UserID` XML attribute.
    #[serde(rename = "@UserID", default)]
    pub user_id: i32,

    /// Name of the software that modified this revision.
    ///
    /// Corresponds to the `ModifiedBy` XML attribute.
    #[serde(rename = "@ModifiedBy", skip_serializing_if = "str::is_empty", default)]
    pub modified_by: String,
}
