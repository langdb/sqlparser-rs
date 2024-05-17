// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#[cfg(not(feature = "std"))]
use alloc::{boxed::Box, format, string::String, vec::Vec};
use core::fmt;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[cfg(feature = "visitor")]
use sqlparser_derive::{Visit, VisitMut};

use crate::ast::ObjectName;

/// SQL data types
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "visitor", derive(Visit, VisitMut))]
pub enum DataType {
    /// Boolean
    Boolean,
    /// Fixed-length character type
    FixedString(u64),
    /// String type
    String,
    /// UUID type
    Uuid,
    /// Int8 type
    Int8,
    /// Int16 type
    Int16,
    /// Int32 type
    Int32,
    /// Int64 type
    Int64,
    /// UInt8 type
    UInt8,
    /// UInt16 type
    UInt16,
    /// UInt32 type
    UInt32,
    /// UInt64 type
    UInt64,
    /// Float32 type
    Float32,
    /// Float64 type
    Float64,
    /// Decimal type with precision and scale
    Decimal(ExactNumberInfo),
    /// Date type
    Date,
    /// DateTime type with optional timezone
    DateTime(Option<String>),
    /// Enum8 type
    Enum8(Vec<EnumType<i8>>),
    /// Enum16 type
    Enum16(Vec<EnumType<i16>>),
    /// Array type
    Array(Box<DataType>),
    /// Nested type
    Nested(Vec<NestedType>),
    /// Tuple type
    Tuple(Vec<DataType>),
    /// Map type
    Map(Box<DataType>, Box<DataType>),
    /// Nullable type
    Nullable(Box<DataType>),
    /// LowCardinality type
    LowCardinality(Box<DataType>),
    /// SimpleAggregateFunction type
    SimpleAggregateFunction(String, Box<DataType>),
    /// AggregateFunction type
    AggregateFunction(String, Vec<DataType>),
    /// Interval type
    Interval(String),
    /// Nothing type
    Nothing,
    /// Custom type
    Custom(ObjectName, Vec<String>),
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "visitor", derive(Visit, VisitMut))]
pub struct NestedType {
    pub name: String,
    pub field_type: Box<DataType>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "visitor", derive(Visit, VisitMut))]
pub struct EnumType<T> {
    pub name: String,
    pub value: T,
}

impl fmt::Display for DataType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DataType::FixedString(length) => write!(f, "FixedString({})", length),
            DataType::Boolean => write!(f, "Boolean"),
            DataType::String => write!(f, "String"),
            DataType::Uuid => write!(f, "UUID"),
            DataType::Int8 => write!(f, "Int8"),
            DataType::Int16 => write!(f, "Int16"),
            DataType::Int32 => write!(f, "Int32"),
            DataType::Int64 => write!(f, "Int64"),
            DataType::UInt8 => write!(f, "UInt8"),
            DataType::UInt16 => write!(f, "UInt16"),
            DataType::UInt32 => write!(f, "UInt32"),
            DataType::UInt64 => write!(f, "UInt64"),
            DataType::Float32 => write!(f, "Float32"),
            DataType::Float64 => write!(f, "Float64"),
            DataType::Decimal(info) => {
                write!(f, "DECIMAL{info}")
            }
            DataType::Date => write!(f, "Date"),
            DataType::DateTime(Some(timezone)) => write!(f, "DateTime('{}')", timezone),
            DataType::DateTime(None) => write!(f, "DateTime"),
            DataType::Enum8(values) => {
                write!(f, "Enum8(")?;
                for (i, value) in values.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "'{}' = {}", value.name, value.value)?;
                }
                write!(f, ")")
            }
            DataType::Enum16(values) => {
                write!(f, "Enum16(")?;
                for (i, value) in values.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "'{}' = {}", value.name, value.value)?;
                }
                write!(f, ")")
            }
            DataType::Array(elem_type) => write!(f, "Array({})", elem_type),
            DataType::Nested(fields) => {
                write!(f, "Nested(")?;
                for (i, nested) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{} {}", nested.name, nested.field_type)?;
                }
                write!(f, ")")
            }
            DataType::Tuple(types) => {
                write!(f, "Tuple(")?;
                for (i, t) in types.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", t)?;
                }
                write!(f, ")")
            }
            DataType::Map(key_type, value_type) => write!(f, "Map({}, {})", key_type, value_type),
            DataType::Nullable(inner_type) => write!(f, "Nullable({})", inner_type),
            DataType::LowCardinality(inner_type) => write!(f, "LowCardinality({})", inner_type),
            DataType::SimpleAggregateFunction(func, inner_type) => {
                write!(f, "SimpleAggregateFunction({}, {})", func, inner_type)
            }
            DataType::AggregateFunction(func, param_types) => {
                write!(f, "AggregateFunction({}", func)?;
                if !param_types.is_empty() {
                    write!(f, ", ")?;
                    for (i, t) in param_types.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", t)?;
                    }
                }
                write!(f, ")")
            }
            DataType::Interval(interval_type) => write!(f, "Interval({})", interval_type),
            DataType::Nothing => write!(f, "Nothing"),
            DataType::Custom(ty, modifiers) => {
                if modifiers.is_empty() {
                    write!(f, "{ty}")
                } else {
                    write!(f, "{}({})", ty, modifiers.join(", "))
                }
            }
        }
    }
}

/// Timestamp and Time data types information about TimeZone formatting.
///
/// This is more related to a display information than real differences between each variant. To
/// guarantee compatibility with the input query we must maintain its exact information.
#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "visitor", derive(Visit, VisitMut))]
pub enum TimezoneInfo {
    /// No information about time zone. E.g., TIMESTAMP
    None,
    /// Temporal type 'WITH TIME ZONE'. E.g., TIMESTAMP WITH TIME ZONE, [standard], [Oracle]
    ///
    /// [standard]: https://jakewheat.github.io/sql-overview/sql-2016-foundation-grammar.html#datetime-type
    /// [Oracle]: https://docs.oracle.com/en/database/oracle/oracle-database/12.2/nlspg/datetime-data-types-and-time-zone-support.html#GUID-3F1C388E-C651-43D5-ADBC-1A49E5C2CA05
    WithTimeZone,
    /// Temporal type 'WITHOUT TIME ZONE'. E.g., TIME WITHOUT TIME ZONE, [standard], [Postgresql]
    ///
    /// [standard]: https://jakewheat.github.io/sql-overview/sql-2016-foundation-grammar.html#datetime-type
    /// [Postgresql]: https://www.postgresql.org/docs/current/datatype-datetime.html
    WithoutTimeZone,
    /// Postgresql specific `WITH TIME ZONE` formatting, for both TIME and TIMESTAMP. E.g., TIMETZ, [Postgresql]
    ///
    /// [Postgresql]: https://www.postgresql.org/docs/current/datatype-datetime.html
    Tz,
}

impl fmt::Display for TimezoneInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TimezoneInfo::None => {
                write!(f, "")
            }
            TimezoneInfo::WithTimeZone => {
                write!(f, " WITH TIME ZONE")
            }
            TimezoneInfo::WithoutTimeZone => {
                write!(f, " WITHOUT TIME ZONE")
            }
            TimezoneInfo::Tz => {
                // TZ is the only one that is displayed BEFORE the precision, so the datatype display
                // must be aware of that. Check <https://www.postgresql.org/docs/14/datatype-datetime.html>
                // for more information
                write!(f, "TZ")
            }
        }
    }
}

/// Additional information for `NUMERIC`, `DECIMAL`, and `DEC` data types
/// following the 2016 [standard].
///
/// [standard]: https://jakewheat.github.io/sql-overview/sql-2016-foundation-grammar.html#exact-numeric-type
#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "visitor", derive(Visit, VisitMut))]
pub enum ExactNumberInfo {
    /// No additional information e.g. `DECIMAL`
    None,
    /// Only precision information e.g. `DECIMAL(10)`
    Precision(u64),
    /// Precision and scale information e.g. `DECIMAL(10,2)`
    PrecisionAndScale(u64, u64),
}

impl fmt::Display for ExactNumberInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExactNumberInfo::None => {
                write!(f, "")
            }
            ExactNumberInfo::Precision(p) => {
                write!(f, "({p})")
            }
            ExactNumberInfo::PrecisionAndScale(p, s) => {
                write!(f, "({p},{s})")
            }
        }
    }
}

/// Information about [character length][1], including length and possibly unit.
///
/// [1]: https://jakewheat.github.io/sql-overview/sql-2016-foundation-grammar.html#character-length
#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "visitor", derive(Visit, VisitMut))]
pub enum CharacterLength {
    IntegerLength {
        /// Default (if VARYING) or maximum (if not VARYING) length
        length: u64,
        /// Optional unit. If not informed, the ANSI handles it as CHARACTERS implicitly
        unit: Option<CharLengthUnits>,
    },
    /// VARCHAR(MAX) or NVARCHAR(MAX), used in T-SQL (Miscrosoft SQL Server)
    Max,
}

impl fmt::Display for CharacterLength {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CharacterLength::IntegerLength { length, unit } => {
                write!(f, "{}", length)?;
                if let Some(unit) = unit {
                    write!(f, " {unit}")?;
                }
            }
            CharacterLength::Max => {
                write!(f, "MAX")?;
            }
        }
        Ok(())
    }
}

/// Possible units for characters, initially based on 2016 ANSI [standard][1].
///
/// [1]: https://jakewheat.github.io/sql-overview/sql-2016-foundation-grammar.html#char-length-units
#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "visitor", derive(Visit, VisitMut))]
pub enum CharLengthUnits {
    /// CHARACTERS unit
    Characters,
    /// OCTETS unit
    Octets,
}

impl fmt::Display for CharLengthUnits {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Characters => {
                write!(f, "CHARACTERS")
            }
            Self::Octets => {
                write!(f, "OCTETS")
            }
        }
    }
}

/// Represents the data type of the elements in an array (if any) as well as
/// the syntax used to declare the array.
///
/// For example: Bigquery/Hive use `ARRAY<INT>` whereas snowflake uses ARRAY.
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "visitor", derive(Visit, VisitMut))]
pub enum ArrayElemTypeDef {
    /// `ARRAY`
    None,
    /// `ARRAY<INT>`
    AngleBracket(Box<DataType>),
    /// `INT[]` or `INT[2]`
    SquareBracket(Box<DataType>, Option<u64>),
    /// Clickhouse: `ARRAY(`String`)`
    Parenthesis(Box<DataType>),
}

/// Represents the data type of the elements in an array (if any) as well as
/// the syntax used to declare the array.
///
/// For example: Bigquery/Hive use `ARRAY<INT>` whereas snowflake uses ARRAY.
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "visitor", derive(Visit, VisitMut))]
pub enum TupleElemTypeDef {
    /// Clickhouse: `TUPLE(`String`, `String`)`
    Parenthesis(Box<Vec<DataType>>),
}
