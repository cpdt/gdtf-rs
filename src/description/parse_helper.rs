//! Provides parsing helpers to work around internally tagged enums that produce wrong results
//! because of https://github.com/serde-rs/serde/issues/1183.
//!
//! This workaround involves annotating all "primitive" fields of an enum with a custom
//! `deserialize_with` function. The fields must implement [FromStr] or be an Option which contains
//! a value that implements FromStr.
//!
//! For example the following enum:
//! ```no_run
//! use serde::Deserialize;
//!
//! #[derive(Deserialize)]
//! #[serde(tag = "@ComponentType")]
//! enum OutputType {
//!     Simple,
//!     Extended {
//!         #[serde(rename = "@Voltage")]
//!         voltage: Option<f32>,
//!     }
//! }
//! ```
//!
//! Should be adjusted to this:
//! ```no_run
//! use serde::Deserialize;
//! use gdtf::description::parse_helper::Parse;
//!
//! #[derive(Deserialize)]
//! #[serde(tag = "@ComponentType")]
//! enum OutputType {
//!     Simple,
//!     Extended {
//!         #[serde(rename = "@Voltage", deserialize_with = "Parse::deserialize")]
//!         voltage: Option<f32>,
//!     }
//! }
//! ```
//!
//! Note: support for each data-type must be opted into due to Rust rules. That can be done by
//! implementing the [CanParse] trait for each type, in this file. For example:
//!
//! ```no_run
//! use gdtf::description::parse_helper::CanParse;
//!
//! impl CanParse for f64 {}
//! ```

use serde::de::{Error, Unexpected, Visitor};
use serde::Deserializer;
use std::fmt::Formatter;
use std::marker::PhantomData;
use std::str::FromStr;

pub trait CanParse: FromStr {}
impl CanParse for f32 {}

pub trait Parse<'de>: Sized {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>;
}

impl<'de, T> Parse<'de> for T
where
    T: CanParse,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(ParseVisitor {
            marker: PhantomData,
        })
    }
}

impl<'de, T> Parse<'de> for Option<T>
where
    T: Parse<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_option(ParseOptVisitor {
            marker: PhantomData,
        })
    }
}

struct ParseVisitor<T> {
    marker: PhantomData<T>,
}

impl<'de, T> Visitor<'de> for ParseVisitor<T>
where
    T: CanParse,
{
    type Value = T;

    fn expecting(&self, formatter: &mut Formatter) -> std::fmt::Result {
        formatter.write_str(std::any::type_name::<T>())
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: Error,
    {
        T::from_str(v).map_err(|_| E::invalid_value(Unexpected::Str(v), &self))
    }
}

struct ParseOptVisitor<T> {
    marker: PhantomData<T>,
}

impl<'de, T> Visitor<'de> for ParseOptVisitor<T>
where
    T: Parse<'de>,
{
    type Value = Option<T>;

    fn expecting(&self, formatter: &mut Formatter) -> std::fmt::Result {
        formatter.write_str("option")
    }

    fn visit_none<E>(self) -> Result<Self::Value, E>
    where
        E: Error,
    {
        Ok(None)
    }

    fn visit_some<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        T::deserialize(deserializer).map(Some)
    }
}
