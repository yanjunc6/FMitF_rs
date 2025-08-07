// src/verification/partition_verification.rs
//! Partition verification for table accesses.
//!
//! This module verifies that table accesses within the same partition are correct
//! and logs information about cross-partition accesses.

use crate::cfg::{
    CfgProgram, FunctionId, FunctionType, HopId, LValue, Operand, Rvalue, Statement, TableId,
};
use crate::dataflow::AccessType;
use crate::sc_graph::{SCGraph, SCGraphNodeId};
use std::path::Path;

/// Detailed table access information including partition data
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DetailedTableAccess {
    pub table_id: TableId,
    pub access_type: AccessType,
    pub pk_values: Vec<Operand>,
    pub hop_id: HopId,
    pub function_id: FunctionId,
    pub instance: u32,
    pub line_info: String, // For logging purposes
}

/// Cross-partition access information (when tables use different partition functions)
#[derive(Debug, Clone)]
pub struct CrossPartitionAccess {
    pub access1: DetailedTableAccess,
    pub access2: DetailedTableAccess,
    pub description: String,
}

/// Partition verification errors (same partition function but different arguments)
#[derive(Debug, Clone)]
pub struct PartitionVerificationError {
    pub access1: DetailedTableAccess,
    pub access2: DetailedTableAccess,
    pub partition_function_name: String,
    pub error_message: String,
}

/// Result of partition verification
#[derive(Debug)]
pub struct PartitionVerificationResult {
    pub verified_accesses: Vec<(DetailedTableAccess, DetailedTableAccess)>,
    pub cross_partition_accesses: Vec<CrossPartitionAccess>,
    pub verification_errors: Vec<PartitionVerificationError>,
    pub boogie_files_generated: usize,
}

/// Main partition verification manager
pub struct PartitionVerifier {
    pub detailed_accesses: Vec<DetailedTableAccess>,
    pub cross_partition_accesses: Vec<CrossPartitionAccess>,
    pub verification_errors: Vec<PartitionVerificationError>,
    pub boogie_output_dir: Option<String>,
}

impl PartitionVerifier {
    pub fn new() -> Self {
        Self {
            detailed_accesses: Vec::new(),
            cross_partition_accesses: Vec::new(),
            verification_errors: Vec::new(),
            boogie_output_dir: None,
        }
    }

    /// Set the output directory for Boogie files
    pub fn set_boogie_output_dir(&mut self, dir: String) {
        self.boogie_output_dir = Some(dir);
    }

    /// Extract detailed table accesses from the SC-graph and CFG program
    pub fn extract_table_accesses(&mut self, cfg_program: &CfgProgram, _sc_graph: &SCGraph) {
        self.detailed_accesses.clear();

        // Iterate through all transaction functions directly from cfg_program
        for (function_id, function) in cfg_program.functions.iter() {
            // Only analyze transaction functions
            if function.function_type != FunctionType::Transaction {
                continue;
            }

            // Extract table accesses from all hops in the function
            for &hop_id in crate::cfg::cfg_api::get_function_hops(cfg_program, function_id) {
                // Create a node ID for this hop (using instance 0 as default)
                let node_id = SCGraphNodeId {
                    function_id,
                    hop_id,
                    instance: 0,
                };

                // Extract table accesses from all basic blocks in the hop
                for &block_id in crate::cfg::cfg_api::get_hop_blocks(cfg_program, hop_id) {
                    for (stmt_index, statement) in
                        crate::cfg::cfg_api::get_block_statements(cfg_program, block_id)
                            .iter()
                            .enumerate()
                    {
                        self.extract_accesses_from_statement(
                            statement,
                            &node_id,
                            cfg_program,
                            format!("{}:{}:{}", function.name, hop_id.index(), stmt_index),
                        );
                    }
                }
            }
        }
    }

    /// Extract table accesses from a single statement
    fn extract_accesses_from_statement(
        &mut self,
        statement: &Statement,
        node_id: &SCGraphNodeId,
        cfg_program: &CfgProgram,
        line_info: String,
    ) {
        match statement {
            Statement::Assign { lvalue, rvalue, .. } => {
                // Check for table reads in rvalue
                self.extract_reads_from_rvalue(rvalue, node_id, cfg_program, &line_info);

                // Check for table writes in lvalue
                self.extract_writes_from_lvalue(lvalue, node_id, cfg_program, &line_info);
            }
        }
    }

    /// Extract table reads from Rvalue
    fn extract_reads_from_rvalue(
        &mut self,
        rvalue: &Rvalue,
        node_id: &SCGraphNodeId,
        _cfg_program: &CfgProgram,
        line_info: &str,
    ) {
        match rvalue {
            Rvalue::TableAccess {
                table, pk_values, ..
            } => {
                self.detailed_accesses.push(DetailedTableAccess {
                    table_id: *table,
                    access_type: AccessType::Read,
                    pk_values: pk_values.clone(),
                    hop_id: node_id.hop_id,
                    function_id: node_id.function_id,
                    instance: node_id.instance,
                    line_info: format!("{} (READ)", line_info),
                });
            }
            // Handle other rvalue types that might contain table accesses
            Rvalue::BinaryOp { left, right, .. } => {
                // Recursively check operands
                self.extract_reads_from_operand(left, node_id, _cfg_program, line_info);
                self.extract_reads_from_operand(right, node_id, _cfg_program, line_info);
            }
            Rvalue::UnaryOp { operand, .. } => {
                self.extract_reads_from_operand(operand, node_id, _cfg_program, line_info);
            }
            _ => {
                // Other rvalue types don't directly access tables
            }
        }
    }

    /// Extract table reads from Operand (if it's a table access)
    fn extract_reads_from_operand(
        &mut self,
        _operand: &Operand,
        _node_id: &SCGraphNodeId,
        _cfg_program: &CfgProgram,
        _line_info: &str,
    ) {
        // Operands typically don't contain direct table accesses in our current CFG design
        // This is a placeholder for future extension if needed
    }

    /// Extract table writes from LValue
    fn extract_writes_from_lvalue(
        &mut self,
        lvalue: &LValue,
        node_id: &SCGraphNodeId,
        _cfg_program: &CfgProgram,
        line_info: &str,
    ) {
        match lvalue {
            LValue::TableField {
                table, pk_values, ..
            } => {
                self.detailed_accesses.push(DetailedTableAccess {
                    table_id: *table,
                    access_type: AccessType::Write,
                    pk_values: pk_values.clone(),
                    hop_id: node_id.hop_id,
                    function_id: node_id.function_id,
                    instance: node_id.instance,
                    line_info: format!("{} (WRITE)", line_info),
                });
            }
            _ => {
                // Other lvalue types don't modify tables
            }
        }
    }

    /// Verify partition consistency for conflicting table accesses
    pub fn verify_partition_consistency(&mut self, cfg_program: &CfgProgram, sc_graph: &SCGraph) {
        self.cross_partition_accesses.clear();
        self.verification_errors.clear();

        // Find conflicting pairs from C-edges
        let c_edges: Vec<_> = sc_graph
            .edges
            .iter()
            .filter(|edge| edge.edge_type == crate::sc_graph::EdgeType::C)
            .collect();

        // Collect all verification items first to avoid borrowing conflicts
        let mut all_verification_items = Vec::new();

        for edge in c_edges {
            // Find all table accesses for both nodes in the C-edge
            let accesses1 = self.get_accesses_for_node(&edge.source);
            let accesses2 = self.get_accesses_for_node(&edge.target);

            // Check for conflicts between the accesses
            for access1 in &accesses1 {
                for access2 in &accesses2 {
                    if access1.table_id == access2.table_id {
                        // Same table accessed - check partition consistency
                        all_verification_items.push((access1.clone(), access2.clone(), true));
                    } else {
                        // Different tables accessed - check if they use same partition function
                        all_verification_items.push((access1.clone(), access2.clone(), false));
                    }
                }
            }
        }

        // Process verification items
        for (access1, access2, is_same_table) in all_verification_items {
            if is_same_table {
                self.verify_same_table_partition(&access1, &access2, cfg_program);
            } else {
                self.check_cross_table_partition(&access1, &access2, cfg_program);
            }
        }
    }

    /// Get all table accesses for a specific SC-graph node
    fn get_accesses_for_node(&self, node_id: &SCGraphNodeId) -> Vec<DetailedTableAccess> {
        self.detailed_accesses
            .iter()
            .filter(|access| {
                access.function_id == node_id.function_id
                    && access.hop_id == node_id.hop_id
                    && access.instance == node_id.instance
            })
            .cloned()
            .collect()
    }

    /// Verify that two accesses to the same table use consistent partition arguments
    fn verify_same_table_partition(
        &mut self,
        access1: &DetailedTableAccess,
        access2: &DetailedTableAccess,
        cfg_program: &CfgProgram,
    ) {
        let table = &cfg_program.tables[access1.table_id];
        let partition_function = &cfg_program.functions[table.partition_function];

        // Check if the partition arguments are the same
        if !self.are_partition_arguments_equal(&access1.pk_values, &access2.pk_values) {
            // Generate Boogie verification for this case
            self.verification_errors.push(PartitionVerificationError {
                access1: access1.clone(),
                access2: access2.clone(),
                partition_function_name: partition_function.name.clone(),
                error_message: format!(
                    "Conflicting accesses to table '{}' with different partition arguments",
                    table.name
                ),
            });
        }
    }

    /// Check if two different tables use the same partition function
    fn check_cross_table_partition(
        &mut self,
        access1: &DetailedTableAccess,
        access2: &DetailedTableAccess,
        cfg_program: &CfgProgram,
    ) {
        let table1 = &cfg_program.tables[access1.table_id];
        let table2 = &cfg_program.tables[access2.table_id];

        if table1.partition_function == table2.partition_function {
            // Same partition function - verify arguments are equal
            if !self.are_partition_arguments_equal(&access1.pk_values, &access2.pk_values) {
                let partition_function = &cfg_program.functions[table1.partition_function];
                self.verification_errors.push(PartitionVerificationError {
                    access1: access1.clone(),
                    access2: access2.clone(),
                    partition_function_name: partition_function.name.clone(),
                    error_message: format!(
                        "Tables '{}' and '{}' use same partition function '{}' but with different arguments",
                        table1.name, table2.name, partition_function.name
                    ),
                });
            }
        } else {
            // Different partition functions - record as cross-partition access
            self.cross_partition_accesses.push(CrossPartitionAccess {
                access1: access1.clone(),
                access2: access2.clone(),
                description: format!(
                    "Cross-partition access: {} -> {} using different partition functions",
                    table1.name, table2.name
                ),
            });
        }
    }

    /// Check if two sets of partition arguments are equal (should be verified with Boogie)
    fn are_partition_arguments_equal(&self, _args1: &[Operand], _args2: &[Operand]) -> bool {
        // For now, we assume they need Boogie verification
        // In the future, we might do simple constant checking here
        false // Always require Boogie verification
    }

    /// Generate Boogie files for verification errors
    pub fn generate_boogie_files(&self, cfg_program: &CfgProgram) -> Result<usize, String> {
        let mut files_generated = 0;

        // Create boogie output directory if specified
        if let Some(ref output_dir) = self.boogie_output_dir {
            std::fs::create_dir_all(output_dir)
                .map_err(|e| format!("Failed to create Boogie output directory: {}", e))?;

            for (index, error) in self.verification_errors.iter().enumerate() {
                let boogie_code = self.generate_boogie_for_verification_error(error, cfg_program);
                let filename = format!("partition_verification_{}.bpl", index);
                let filepath = Path::new(output_dir).join(&filename);

                std::fs::write(&filepath, boogie_code).map_err(|e| {
                    format!("Failed to write Boogie file {}: {}", filepath.display(), e)
                })?;

                files_generated += 1;
            }
        }

        Ok(files_generated)
    }

    /// Generate Boogie code for a partition verification error
    fn generate_boogie_for_verification_error(
        &self,
        error: &PartitionVerificationError,
        cfg_program: &CfgProgram,
    ) -> String {
        let table1 = &cfg_program.tables[error.access1.table_id];
        let table2 = &cfg_program.tables[error.access2.table_id];
        let _partition_func = &cfg_program.functions[table1.partition_function];

        let mut code = String::new();

        // Header comment
        code.push_str(&format!(
            "// Partition verification for {}\n",
            error.partition_function_name
        ));
        code.push_str(&format!(
            "// Access 1: {} ({})\n",
            table1.name, error.access1.line_info
        ));
        code.push_str(&format!(
            "// Access 2: {} ({})\n",
            table2.name, error.access2.line_info
        ));
        code.push_str("// Verifying that partition arguments are equal\n\n");

        // Generate procedure
        code.push_str("procedure verify_partition_equality(\n");

        // Add parameters for both accesses
        let param_count = error
            .access1
            .pk_values
            .len()
            .max(error.access2.pk_values.len());
        for i in 0..param_count {
            if i > 0 {
                code.push_str(",\n");
            }
            code.push_str(&format!("    a{}: int, b{}: int", i, i));
        }
        code.push_str("\n) {\n");

        // Generate assertions for equality
        for i in 0..param_count {
            code.push_str(&format!("    assert a{} == b{};\n", i, i));
        }

        code.push_str("}\n");
        code
    }

    /// Run the complete partition verification pipeline
    pub fn run_verification(
        &mut self,
        cfg_program: &CfgProgram,
        sc_graph: &SCGraph,
    ) -> PartitionVerificationResult {
        // Step 1: Extract detailed table accesses
        self.extract_table_accesses(cfg_program, sc_graph);

        // Step 2: Verify partition consistency
        self.verify_partition_consistency(cfg_program, sc_graph);

        // Step 3: Generate Boogie files
        let boogie_files_generated = self.generate_boogie_files(cfg_program).unwrap_or_else(|e| {
            eprintln!("Warning: Failed to generate Boogie files: {}", e);
            0
        });

        PartitionVerificationResult {
            verified_accesses: Vec::new(), // Placeholder for successful verifications
            cross_partition_accesses: self.cross_partition_accesses.clone(),
            verification_errors: self.verification_errors.clone(),
            boogie_files_generated,
        }
    }
}
