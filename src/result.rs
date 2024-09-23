use quick_xml::DeError;
use serde_path_to_error::Error;
use std::fmt::{Display, Formatter};
use thiserror::Error;
use zip::result::ZipError;

/// Generic result type with [GdtfError] as its error variant
pub type GdtfResult<T> = Result<T, GdtfError>;

/// Error type for operations with [GdtfFiles](crate::GdtfFile).
#[derive(Debug, Error)]
#[non_exhaustive]
pub enum GdtfError {
    /// I/o error.
    Io(#[from] std::io::Error),

    /// Invalid XML.
    InvalidXml(Box<dyn std::error::Error + Send + Sync>),

    /// Specified resource not found in file.
    ResourceNotFound,
}

impl From<GdtfError> for std::io::Error {
    fn from(err: GdtfError) -> Self {
        let kind = match &err {
            GdtfError::Io(err) => err.kind(),
            GdtfError::InvalidXml(_) => std::io::ErrorKind::InvalidData,
            GdtfError::ResourceNotFound => std::io::ErrorKind::NotFound,
        };

        std::io::Error::new(kind, err)
    }
}

impl From<ZipError> for GdtfError {
    fn from(value: ZipError) -> Self {
        GdtfError::Io(value.into())
    }
}

impl From<DeError> for GdtfError {
    fn from(value: DeError) -> Self {
        GdtfError::InvalidXml(Box::new(value))
    }
}

impl From<serde_path_to_error::Error<DeError>> for GdtfError {
    fn from(value: Error<DeError>) -> Self {
        GdtfError::InvalidXml(Box::new(value))
    }
}

impl Display for GdtfError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            GdtfError::Io(io) => write!(f, "i/o error: {io}"),
            GdtfError::InvalidXml(inner) => write!(f, "invalid xml: {inner}"),
            GdtfError::ResourceNotFound => write!(f, "specified resource not found in file"),
        }
    }
}
