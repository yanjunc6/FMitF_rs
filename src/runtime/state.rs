//! Runtime State - One struct with CFG + simple maps for everything

use crate::{
    ast::parse_and_analyze,
    cfg::{CfgBuilder, CfgProgram, FunctionId, TableId, FieldId},
};
use super::{RuntimeValue, RuntimeError};
use std::collections::HashMap;

/// Main runtime state - keeps everything simple for testing
pub struct RuntimeState {
    /// The loaded CFG program
    pub cfg_program: Option<CfgProgram>,
    
    /// String to ID lookups (built from CFG)
    pub functions: HashMap<String, FunctionId>,
    pub tables: HashMap<String, TableId>,
    pub fields: HashMap<String, FieldId>,
    
    /// Table data: TableId → (PrimaryKey → (FieldId → RuntimeValue))
    /// PrimaryKey = Vec<RuntimeValue> (no string concatenation!)
    pub table_data: HashMap<TableId, HashMap<Vec<RuntimeValue>, HashMap<FieldId, RuntimeValue>>>,
}

impl RuntimeState {
    /// Create new empty runtime state
    pub fn new() -> Self {
        Self {
            cfg_program: None,
            functions: HashMap::new(),
            tables: HashMap::new(),
            fields: HashMap::new(),
            table_data: HashMap::new(),
        }
    }
    
    /// Load a TransAct file and build CFG
    pub fn load_file(&mut self, file_path: &str) -> Result<(), RuntimeError> {
        // Read file
        let source = std::fs::read_to_string(file_path)
            .map_err(|e| RuntimeError::ParseError(format!("Failed to read file: {}", e)))?;
        
        // Parse and analyze
        let ast_program = parse_and_analyze(&source)
            .map_err(|errors| RuntimeError::ParseError(format!("Parse errors: {:?}", errors)))?;
        
        // Build CFG
        let cfg_ctx = CfgBuilder::build_from_program(&ast_program)
            .map_err(|e| RuntimeError::ParseError(format!("CFG build error: {}", e)))?;
        
        let cfg_program = cfg_ctx.program;
        
        // Build string→ID lookup tables
        self.build_symbol_tables(&cfg_program);
        
        // Initialize empty table storage
        self.initialize_table_storage(&cfg_program);
        
        // Store CFG
        self.cfg_program = Some(cfg_program);
        
        println!("✅ Loaded program with {} functions, {} tables", 
            self.functions.len(), self.tables.len());
        
        Ok(())
    }
    
    /// Call a function by name with string arguments
    pub fn call_function(&mut self, function_name: &str, args: Vec<String>) -> Result<(), RuntimeError> {
        let func_id = self.functions.get(function_name)
            .copied()
            .ok_or_else(|| RuntimeError::NotFound(format!("Function '{}'", function_name)))?;
        
        // Parse string args to RuntimeValue (simple: try int first, then string)
        let parsed_args: Vec<RuntimeValue> = args.into_iter()
            .map(|arg| {
                if let Ok(int_val) = arg.parse::<i64>() {
                    RuntimeValue::Int(int_val)
                } else if arg == "true" {
                    RuntimeValue::Bool(true)
                } else if arg == "false" {
                    RuntimeValue::Bool(false)
                } else {
                    RuntimeValue::String(arg)
                }
            })
            .collect();
        
        // Execute function (call executor)
        super::executor::execute_function(self, func_id, parsed_args)
    }
    
    /// Get table data for display
    pub fn get_table_data(&self, table_name: &str) -> Option<&HashMap<Vec<RuntimeValue>, HashMap<FieldId, RuntimeValue>>> {
        let table_id = self.tables.get(table_name)?;
        self.table_data.get(table_id)
    }
    
    /// Print table in aligned format
    pub fn print_table(&self, table_name: &str) -> Result<(), RuntimeError> {
        let table_id = self.tables.get(table_name)
            .copied()
            .ok_or_else(|| RuntimeError::NotFound(format!("Table '{}'", table_name)))?;
        
        let cfg = self.cfg_program.as_ref()
            .ok_or_else(|| RuntimeError::ExecutionError("No program loaded".to_string()))?;
        
        let table_info = &cfg.tables[table_id];
        let data = self.table_data.get(&table_id)
            .ok_or_else(|| RuntimeError::NotFound(format!("Table data for '{}'", table_name)))?;
        
        if data.is_empty() {
            println!("Table {} is empty.", table_name);
            return Ok(());
        }
        
        // Calculate column widths
        let mut column_widths = Vec::new();
        let mut column_headers = Vec::new();
        
        for &field_id in &table_info.fields {
            let field_name = &cfg.fields[field_id].name;
            let header = if table_info.primary_keys.contains(&field_id) {
                format!("{} (PK)", field_name)
            } else {
                field_name.clone()
            };
            
            // Start with header width, minimum 6 characters for readability
            let mut max_width = header.len().max(6);
            
            // Check data widths
            for (pk_values, field_data) in data {
                let mut field_idx = 0;
                for &check_field_id in &table_info.fields {
                    if check_field_id == field_id {
                        let value_str = if table_info.primary_keys.contains(&field_id) {
                            if field_idx < pk_values.len() {
                                pk_values[field_idx].to_string()
                            } else {
                                "NULL".to_string()
                            }
                        } else {
                            if let Some(value) = field_data.get(&field_id) {
                                value.to_string()
                            } else {
                                "NULL".to_string()
                            }
                        };
                        max_width = max_width.max(value_str.len());
                        break;
                    }
                    if table_info.primary_keys.contains(&check_field_id) {
                        field_idx += 1;
                    }
                }
            }
            
            column_headers.push(header);
            column_widths.push(max_width);
        }
        
        // Print header row
        print!("┌");
        for (i, width) in column_widths.iter().enumerate() {
            print!("{}", "─".repeat(width + 2));
            if i < column_widths.len() - 1 {
                print!("┬");
            }
        }
        println!("┐");
        
        print!("│");
        for (i, (header, width)) in column_headers.iter().zip(&column_widths).enumerate() {
            print!(" {:<width$} ", header, width = width);
            if i < column_widths.len() - 1 {
                print!("│");
            }
        }
        println!("│");
        
        // Print separator
        print!("├");
        for (i, width) in column_widths.iter().enumerate() {
            print!("{}", "─".repeat(width + 2));
            if i < column_widths.len() - 1 {
                print!("┼");
            }
        }
        println!("┤");
        
        // Print data rows
        for (pk_values, field_data) in data {
            print!("│");
            let mut field_idx = 0;
            
            for (col_idx, &field_id) in table_info.fields.iter().enumerate() {
                let value_str = if table_info.primary_keys.contains(&field_id) {
                    if field_idx < pk_values.len() {
                        let s = pk_values[field_idx].to_string();
                        field_idx += 1;
                        s
                    } else {
                        "NULL".to_string()
                    }
                } else {
                    if let Some(value) = field_data.get(&field_id) {
                        value.to_string()
                    } else {
                        "NULL".to_string()
                    }
                };
                
                let width = column_widths[col_idx];
                print!(" {:<width$} ", value_str, width = width);
                if col_idx < table_info.fields.len() - 1 {
                    print!("│");
                }
            }
            println!("│");
        }
        
        // Print bottom border
        print!("└");
        for (i, width) in column_widths.iter().enumerate() {
            print!("{}", "─".repeat(width + 2));
            if i < column_widths.len() - 1 {
                print!("┴");
            }
        }
        println!("┘");
        
        Ok(())
    }
    
    /// List all available functions
    pub fn list_functions(&self) -> Vec<String> {
        self.functions.keys().cloned().collect()
    }
    
    /// List all available tables
    pub fn list_tables(&self) -> Vec<String> {
        self.tables.keys().cloned().collect()
    }
    
    /// Build string→ID lookup tables from CFG
    fn build_symbol_tables(&mut self, cfg: &CfgProgram) {
        self.functions.clear();
        self.tables.clear();
        self.fields.clear();
        
        // Functions
        for (func_id, func_info) in cfg.functions.iter() {
            self.functions.insert(func_info.name.clone(), func_id);
        }
        
        // Tables
        for (table_id, table_info) in cfg.tables.iter() {
            self.tables.insert(table_info.name.clone(), table_id);
        }
        
        // Fields
        for (field_id, field_info) in cfg.fields.iter() {
            self.fields.insert(field_info.name.clone(), field_id);
        }
    }
    
    /// Initialize empty table storage
    fn initialize_table_storage(&mut self, cfg: &CfgProgram) {
        self.table_data.clear();
        
        for (table_id, _table_info) in cfg.tables.iter() {
            self.table_data.insert(table_id, HashMap::new());
        }
    }
}