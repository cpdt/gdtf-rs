//! Fixture type specific presets.

use serde::{Deserialize, Serialize};

/// Defines fixture type specific presets.
///
/// Has not yet been defined by the GDTF specification.
///
/// Corresponds to an `<FTPreset>` XML node.
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct FtPreset;
