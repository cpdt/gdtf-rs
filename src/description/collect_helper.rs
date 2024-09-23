/// Defines serialize and deserialize functions to handle "collect" XML nodes like this:
/// ```xml
/// <FixtureType>
///     <Revisions>
///         <Revision />
///         <Revision />
///         <Revision />
///     </Revisions>
/// </FixtureType>
/// ```
/// can become:
/// ```text
/// FixtureType {
///     revisions: [ Revision, Revision, Revision ]
/// }
/// ```
///
/// Can also handle multiple collect nodes, appending them into a single list:
/// ```xml
/// <FixtureType>
///     <Revisions>
///         <Revision />
///     </Revisions>
///     <Revisions>
///         <Revision />
///     </Revisions>
/// </FixtureType>
/// ```
/// can become:
/// ```text
/// FixtureType {
///     revisions: [ Revision, Revision ]
/// }
/// ```
macro_rules! define_collect_helper {
    ($tag:literal ($serialize:ident, $deserialize:ident) -> $valtype:ty) => {
        fn $serialize<S>(value: &[$valtype], serializer: S) -> Result<S::Ok, S::Error>
        where
            S: ::serde::Serializer,
        {
            #[derive(::serde::Serialize)]
            struct SeShim<'s> {
                #[serde(rename = $tag)]
                value: &'s [$valtype],
            }
            SeShim { value }.serialize(serializer)
        }
        fn $deserialize<'de, D>(deserializer: D) -> Result<Vec<$valtype>, D::Error>
        where
            D: ::serde::Deserializer<'de>,
        {
            #[derive(::serde::Deserialize)]
            struct DeShim {
                #[serde(rename = $tag, default)]
                value: ::std::vec::Vec<$valtype>,
            }
            ::std::vec::Vec::<DeShim>::deserialize(deserializer).map(|shim_list| {
                shim_list
                    .into_iter()
                    .flat_map(|shim| shim.value.into_iter())
                    .collect()
            })
        }
    };
}
