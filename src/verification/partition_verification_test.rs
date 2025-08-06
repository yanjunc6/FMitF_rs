// src/verification/partition_verification_test.rs
//! Test utilities for partition verification

use crate::verification::PartitionVerifier;
use crate::ast::parse_and_analyze;
use crate::cfg::CfgBuilder;

pub fn test_partition_verification_on_file(filename: &str) -> Result<(), String> {
    // Read the test file
    let source_code = std::fs::read_to_string(filename)
        .map_err(|e| format!("Failed to read file {}: {}", filename, e))?;

    // Parse AST
    let ast_program = parse_and_analyze(&source_code)
        .map_err(|errors| format!("AST parsing failed with {} errors", errors.len()))?;

    // Build CFG
    let cfg_context = CfgBuilder::build_from_program(&ast_program)
        .map_err(|e| format!("CFG building failed: {}", e))?;
    let cfg_program = cfg_context.program;

    // Run partition verification
    let mut partition_verifier = PartitionVerifier::new();
    let result = partition_verifier.run_verification(&cfg_program);

    // Print results
    println!("=== Partition Verification Results for {} ===", filename);
    println!("Table accesses found: {}", partition_verifier.detailed_accesses.len());
    
    for access in &partition_verifier.detailed_accesses {
        let table_name = &cfg_program.tables[access.table_id].name;
        println!("  - {} {} table '{}' at {}", 
            match access.access_type {
                crate::dataflow::AccessType::Read => "READ",
                crate::dataflow::AccessType::Write => "WRITE",
            },
            match access.access_type {
                crate::dataflow::AccessType::Read => "from",
                crate::dataflow::AccessType::Write => "to",
            },
            table_name,
            access.line_info
        );
    }

    println!("Cross-partition accesses: {}", result.cross_partition_accesses.len());
    for cross_access in &result.cross_partition_accesses {
        println!("  - {}", cross_access.description);
    }

    println!("Verification errors: {}", result.verification_errors.len());
    for error in &result.verification_errors {
        println!("  - {}", error.error_message);
    }

    println!("Boogie files generated: {}", result.boogie_files_generated);
    println!();

    Ok(())
}
