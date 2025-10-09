use super::{util::write_go_file_header, GoProgram};
use std::error::Error;
use std::fmt::Write;

pub fn generate_converters() -> Result<GoProgram, Box<dyn Error>> {
    let content = build_converters_source()?;
    Ok(GoProgram::new("converters.go", content))
}

fn build_converters_source() -> Result<String, std::fmt::Error> {
    let mut out = String::new();

    write_go_file_header(&mut out, &["strconv"])?;

    writeln!(out, "// TODO: Add composite/list converters and handle locale or precision policies if required.")?;
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

    Ok(out)
}
