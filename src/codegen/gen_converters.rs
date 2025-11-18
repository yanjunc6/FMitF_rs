use super::{util::write_go_file_header, GoProgram};
use std::error::Error;
use std::fmt::Write;

pub fn generate_converters() -> Result<GoProgram, Box<dyn Error>> {
    let content = build_converters_source()?;
    Ok(GoProgram::new("converters.go", content))
}

fn build_converters_source() -> Result<String, std::fmt::Error> {
    let mut out = String::new();

    write_go_file_header(
        &mut out,
        &["strconv", "encoding/json", "github.com/boltdb/bolt"],
    )?;

    writeln!(out, "// TODO: Add composite/list converters and handle locale or precision policies if required.")?;
    writeln!(out)?;

    // Constants
    writeln!(out, "// __slice__ is a constant used for scan operations")?;
    writeln!(out, "const __slice__ = 0")?;
    writeln!(out)?;

    // UUID type definition
    writeln!(out, "// UUID is a unique identifier type (uint64 for now)")?;
    writeln!(out, "type UUID uint64")?;
    writeln!(out)?;

    // genUUID function
    writeln!(out, "// genUUID generates a unique identifier")?;
    writeln!(
        out,
        "// In a real implementation, this should generate proper UUIDs"
    )?;
    writeln!(out, "// For now, it returns a simple counter-based ID")?;
    writeln!(out, "var uuidCounter uint64 = 1")?;
    writeln!(out, "func genUUID(n int, m int) UUID {{")?;
    writeln!(out, "\t_ = n")?;
    writeln!(out, "\t_ = m")?;
    writeln!(out, "\tuuidCounter++")?;
    writeln!(out, "\treturn UUID(uuidCounter)")?;
    writeln!(out, "}}")?;
    writeln!(out)?;

    // Unit type definition
    writeln!(
        out,
        "// Unit is a user-defined type representing the unit type"
    )?;
    writeln!(out, "type Unit struct {{}}")?;
    writeln!(out)?;

    // to_unit function
    writeln!(
        out,
        "// to_unit converts any value to Unit type (pass-by-reference style)"
    )?;
    writeln!(out, "func to_unit[T any](value T, result *Unit) {{")?;
    writeln!(
        out,
        "\t// This function marks the value as used and assigns to result"
    )?;
    writeln!(out, "\t_ = value")?;
    writeln!(out, "\t*result = Unit{{}}")?;
    writeln!(out, "}}")?;
    writeln!(out)?;

    // String -> typed converters
    writeln!(out, "// toUint64 converts string to uint64")?;
    writeln!(out, "func toUint64(s string) uint64 {{")?;
    writeln!(out, "\tv, err := strconv.ParseUint(s, 10, 64)")?;
    writeln!(out, "\tif err != nil {{")?;
    writeln!(out, "\t\tpanic(err)")?;
    writeln!(out, "\t}}")?;
    writeln!(out, "\treturn v")?;
    writeln!(out, "}}")?;
    writeln!(out)?;

    writeln!(out, "// toInt64 converts string to int64")?;
    writeln!(out, "func toInt64(s string) int64 {{")?;
    writeln!(out, "\tv, err := strconv.ParseInt(s, 10, 64)")?;
    writeln!(out, "\tif err != nil {{")?;
    writeln!(out, "\t\tpanic(err)")?;
    writeln!(out, "\t}}")?;
    writeln!(out, "\treturn v")?;
    writeln!(out, "}}")?;
    writeln!(out)?;

    writeln!(out, "// toFloat32 converts string to float32")?;
    writeln!(out, "func toFloat32(s string) float32 {{")?;
    writeln!(out, "\tf, err := strconv.ParseFloat(s, 32)")?;
    writeln!(out, "\tif err != nil {{")?;
    writeln!(out, "\t\tpanic(err)")?;
    writeln!(out, "\t}}")?;
    writeln!(out, "\treturn float32(f)")?;
    writeln!(out, "}}")?;
    writeln!(out)?;

    writeln!(out, "// toBool converts string to bool")?;
    writeln!(out, "func toBool(s string) bool {{")?;
    writeln!(out, "\tb, err := strconv.ParseBool(s)")?;
    writeln!(out, "\tif err != nil {{")?;
    writeln!(out, "\t\tpanic(err)")?;
    writeln!(out, "\t}}")?;
    writeln!(out, "\treturn b")?;
    writeln!(out, "}}")?;
    writeln!(out)?;

    // Typed -> string converters
    writeln!(out, "// fromUint64 converts uint64 to string")?;
    writeln!(out, "func fromUint64(v uint64) string {{")?;
    writeln!(out, "\treturn strconv.FormatUint(v, 10)")?;
    writeln!(out, "}}")?;
    writeln!(out)?;

    writeln!(out, "// fromInt64 converts int64 to string")?;
    writeln!(out, "func fromInt64(v int64) string {{")?;
    writeln!(out, "\treturn strconv.FormatInt(v, 10)")?;
    writeln!(out, "}}")?;
    writeln!(out)?;

    writeln!(out, "// fromFloat32 converts float32 to string")?;
    writeln!(out, "func fromFloat32(f float32) string {{")?;
    writeln!(out, "\treturn strconv.FormatFloat(float64(f), 'f', -1, 32)")?;
    writeln!(out, "}}")?;
    writeln!(out)?;

    writeln!(out, "// fromBool converts bool to string")?;
    writeln!(out, "func fromBool(b bool) string {{")?;
    writeln!(out, "\treturn strconv.FormatBool(b)")?;
    writeln!(out, "}}")?;
    writeln!(out)?;

    writeln!(
        out,
        "// concat joins any number of string parts without additional separators."
    )?;
    writeln!(out, "func concat(parts ...string) string {{")?;
    writeln!(out, "\tif len(parts) == 0 {{")?;
    writeln!(out, "\t\treturn \"\"")?;
    writeln!(out, "\t}}")?;
    writeln!(out, "\tresult := parts[0]")?;
    writeln!(out, "\tfor i := 1; i < len(parts); i++ {{")?;
    writeln!(out, "\t\tresult += parts[i]")?;
    writeln!(out, "\t}}")?;
    writeln!(out, "\treturn result")?;
    writeln!(out, "}}")?;
    writeln!(out)?;

    writeln!(
        out,
        "// listGet retrieves an item from a slice, panicking if the index is out of range."
    )?;
    writeln!(out, "func listGet[T any](list []T, idx int) T {{")?;
    writeln!(out, "\tif idx < 0 || idx >= len(list) {{")?;
    writeln!(out, "\t\tpanic(\"listGet: index out of range\")")?;
    writeln!(out, "\t}}")?;
    writeln!(out, "\treturn list[idx]")?;
    writeln!(out, "}}")?;
    writeln!(out)?;

    // Iterator type and related functions
    writeln!(out, "// Iterator wraps a BoltDB cursor for table iteration")?;
    writeln!(out, "type Iterator struct {{")?;
    writeln!(out, "\tcursor *bolt.Cursor")?;
    writeln!(out, "\tcurrentKey []byte")?;
    writeln!(out, "\tcurrentValue []byte")?;
    writeln!(out, "\tisValid bool")?;
    writeln!(out, "}}")?;
    writeln!(out)?;

    // scan function - creates an iterator over a table
    writeln!(out, "// scan creates an iterator over a table")?;
    writeln!(
        out,
        "func scan(tx *bolt.Tx, tableName string, n int, m int) *Iterator {{"
    )?;
    writeln!(out, "\t_ = n // unused parameters")?;
    writeln!(out, "\t_ = m")?;
    writeln!(out, "\tb := tx.Bucket([]byte(tableName))")?;
    writeln!(out, "\tif b == nil {{")?;
    writeln!(out, "\t\treturn &Iterator{{isValid: false}}")?;
    writeln!(out, "\t}}")?;
    writeln!(out, "\tc := b.Cursor()")?;
    writeln!(out, "\tk, v := c.First()")?;
    writeln!(out, "\treturn &Iterator{{")?;
    writeln!(out, "\t\tcursor: c,")?;
    writeln!(out, "\t\tcurrentKey: k,")?;
    writeln!(out, "\t\tcurrentValue: v,")?;
    writeln!(out, "\t\tisValid: k != nil,")?;
    writeln!(out, "\t}}")?;
    writeln!(out, "}}")?;
    writeln!(out)?;

    // hasNext function - checks if iterator has more elements
    writeln!(out, "// hasNext checks if the iterator has more elements")?;
    writeln!(out, "func hasNext(iter *Iterator) bool {{")?;
    writeln!(out, "\treturn iter.isValid")?;
    writeln!(out, "}}")?;
    writeln!(out)?;

    // next function - advances the iterator to the next element
    writeln!(out, "// next advances the iterator to the next element")?;
    writeln!(out, "func next(iter *Iterator) *Iterator {{")?;
    writeln!(out, "\tif !iter.isValid {{")?;
    writeln!(out, "\t\treturn iter")?;
    writeln!(out, "\t}}")?;
    writeln!(out, "\tk, v := iter.cursor.Next()")?;
    writeln!(out, "\titer.currentKey = k")?;
    writeln!(out, "\titer.currentValue = v")?;
    writeln!(out, "\titer.isValid = k != nil")?;
    writeln!(out, "\treturn iter")?;
    writeln!(out, "}}")?;
    writeln!(out)?;

    // get_id function - generic function to extract ID from iterator
    // This needs to be generated per table type
    // TODO: delete this function, and generate per-table get_id functions, move it to global.go
    writeln!(
        out,
        "// get_id extracts the UUID from the iterator's current key"
    )?;
    writeln!(
        out,
        "// This is a generic implementation that assumes the key is a UUID"
    )?;
    writeln!(out, "func get_id(iter *Iterator) UUID {{")?;
    writeln!(out, "\tif !iter.isValid {{")?;
    writeln!(out, "\t\tpanic(\"get_id: iterator is not valid\")")?;
    writeln!(out, "\t}}")?;
    writeln!(out, "\t// Deserialize key as UUID")?;
    writeln!(out, "\tvar id UUID")?;
    writeln!(out, "\tjson.Unmarshal(iter.currentKey, &id)")?;
    writeln!(out, "\treturn id")?;
    writeln!(out, "}}")?;

    Ok(out)
}
