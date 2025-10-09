use super::{
    util::{go_type_string, pascal_case, write_go_file_header},
    GoProgram,
};
use crate::cfg;
use std::error::Error;
use std::fmt::Write;

pub(super) fn generate_global(program: &cfg::Program) -> Result<GoProgram, Box<dyn Error>> {
    let content = build_global_source(program)?;
    Ok(GoProgram::new("global.go", content))
}

fn build_global_source(program: &cfg::Program) -> Result<String, std::fmt::Error> {
    let mut out = String::new();

    write_go_file_header(&mut out, &["encoding/json", "github.com/boltdb/bolt"])?;

    writeln!(out, "func mustJSON(value any) []byte {{")?;
    writeln!(out, "\tbytes, err := json.Marshal(value)")?;
    writeln!(out, "\tif err != nil {{")?;
    writeln!(out, "\t\tpanic(err)")?;
    writeln!(out, "\t}}")?;
    writeln!(out, "\treturn bytes")?;
    writeln!(out, "}}")?;
    writeln!(out)?;

    writeln!(
        out,
        "func putData(bucket *bolt.Bucket, key []byte, value []byte) {{"
    )?;
    writeln!(out, "\tif bucket == nil {{")?;
    writeln!(out, "\t\tpanic(\"nil bucket\")")?;
    writeln!(out, "\t}}")?;
    writeln!(out, "\tif err := bucket.Put(key, value); err != nil {{")?;
    writeln!(out, "\t\tpanic(err)")?;
    writeln!(out, "\t}}")?;
    writeln!(out, "}}")?;
    writeln!(out)?;

    for table_id in &program.all_tables {
        let table = &program.tables[*table_id];
        let struct_name = go_type_name(&table.name);
        let key_struct_name = format!("{}Key", struct_name);
        let bucket_name = format!("t{}", struct_name);

        writeln!(out, "type {} struct {{", key_struct_name)?;
        for field_id in &table.primary_key_fields {
            let field = &program.table_fields[*field_id];
            let go_field_name = go_field_name(&field.name);
            let go_type = go_type_string(program, field.field_type);
            writeln!(out, "\t{} {}", go_field_name, go_type)?;
        }
        writeln!(out, "}}")?;
        writeln!(out)?;

        writeln!(out, "type {} struct {{", struct_name)?;
        writeln!(out, "\tKey {}", key_struct_name)?;
        for field_id in &table.other_fields {
            let field = &program.table_fields[*field_id];
            let go_field_name = go_field_name(&field.name);
            let go_type = go_type_string(program, field.field_type);
            writeln!(out, "\t{} {}", go_field_name, go_type)?;
        }
        writeln!(out, "}}")?;
        writeln!(out)?;

        writeln!(
            out,
            "func get{}(tx *bolt.Tx, key {}) ([]byte, {}) {{",
            struct_name, key_struct_name, struct_name
        )?;
        writeln!(out, "\tbucket := tx.Bucket([]byte(\"{}\"))", bucket_name)?;
        writeln!(out, "\tif bucket == nil {{")?;
        writeln!(out, "\t\tpanic(\"missing bucket {}\")", bucket_name)?;
        writeln!(out, "\t}}")?;
        writeln!(out, "\tkeyBytes := mustJSON(key)")?;
        writeln!(out, "\tvar row {}", struct_name)?;
        writeln!(out, "\tif data := bucket.Get(keyBytes); data != nil {{")?;
        writeln!(
            out,
            "\t\tif err := json.Unmarshal(data, &row); err != nil {{"
        )?;
        writeln!(out, "\t\t\tpanic(err)")?;
        writeln!(out, "\t\t}}")?;
        writeln!(out, "\t}}")?;
        writeln!(out, "\trow.Key = key")?;
        writeln!(out, "\treturn keyBytes, row")?;
        writeln!(out, "}}")?;
        writeln!(out)?;

        writeln!(
            out,
            "func put{}(tx *bolt.Tx, key {}, row {}) {{",
            struct_name, key_struct_name, struct_name
        )?;
        writeln!(out, "\tbucket := tx.Bucket([]byte(\"{}\"))", bucket_name)?;
        writeln!(out, "\tif bucket == nil {{")?;
        writeln!(out, "\t\tpanic(\"missing bucket {}\")", bucket_name)?;
        writeln!(out, "\t}}")?;
        writeln!(out, "\trow.Key = key")?;
        writeln!(out, "\tputData(bucket, mustJSON(key), mustJSON(row))")?;
        writeln!(out, "}}")?;
        writeln!(out)?;
    }

    Ok(out)
}

fn go_type_name(name: &str) -> String {
    pascal_case(name)
}

fn go_field_name(name: &str) -> String {
    // Field identifiers can remain as-is as they are already exported (upper-case)
    name.to_string()
}
