//! src/frontend/generated_functions.rs
//!
//! Generates get/set accessor functions for table Row<T> types.
//! This stage runs after AST building but before name resolution.

use crate::ast::common::{Decorator, Identifier};
use crate::ast::item::*;
use crate::ast::*;

/// Generate accessor functions for all table declarations
pub fn generate_table_accessors(program: &mut Program) {
    let table_ids: Vec<TableId> = program.table_decls.iter().map(|(id, _)| id).collect();

    for table_id in table_ids {
        generate_accessors_for_table(program, table_id);
    }
}

/// Generate get/set functions for a specific table
fn generate_accessors_for_table(program: &mut Program, table_id: TableId) {
    let table_decl = program.table_decls[table_id].clone();
    let table_name = table_decl.name.name.clone();

    // Generate functions for each field
    for element in &table_decl.elements {
        if let TableElement::Field(field) = element {
            generate_field_accessors(program, &table_name, field);
        }
    }
}

/// Generate get/set functions for a specific field
fn generate_field_accessors(program: &mut Program, table_name: &str, field: &TableField) {
    let field_name = &field.name.name;

    // Generate getter: get_FieldName(row: Row<TableName>) -> FieldType
    let getter_name = format!("get_{}", field_name);
    let _getter_id = generate_getter_function(program, &getter_name, table_name, field);

    // Generate setter: set_FieldName(row: Row<TableName>, value: FieldType) -> Row<TableName>
    let setter_name = format!("set_{}", field_name);
    let _setter_id = generate_setter_function(program, &setter_name, table_name, field);

    // Store these for potential future use
    // (Could be used for method resolution later)
}

/// Generate a getter function for a field
fn generate_getter_function(
    program: &mut Program,
    function_name: &str,
    table_name: &str,
    field: &TableField,
) -> FunctionId {
    // Create the generic parameter T for the table type
    let generic_param_id = program.generic_params.alloc(GenericParam {
        name: Identifier {
            name: table_name.to_string(),
            span: None,
        },
        span: None,
    });

    // Create Row<T> type for parameter
    let row_base_type = create_identifier_type(program, "Row");
    let table_arg_type = create_identifier_type(program, table_name);
    let row_type = create_generic_type(program, row_base_type, vec![table_arg_type]);

    // Create parameter: row: Row<TableName>
    let param_id = program.params.alloc(Parameter {
        name: Identifier {
            name: "row".to_string(),
            span: None,
        },
        ty: row_type,
        resolved_type: None,
        span: None,
    });

    // Return type is the field type
    let return_type = field.ty;

    // Create the function declaration
    let function_id = program.functions.alloc(CallableDecl {
        kind: CallableKind::Function,
        name: Identifier {
            name: function_name.to_string(),
            span: None,
        },
        decorators: vec![
            Decorator {
                name: Identifier {
                    name: "generated".to_string(),
                    span: None,
                },
                span: None,
            },
            Decorator {
                name: Identifier {
                    name: "intrinsic".to_string(),
                    span: None,
                },
                span: None,
            },
        ],
        generic_params: vec![generic_param_id],
        params: vec![param_id],
        return_type: Some(return_type),
        body: None, // Intrinsic functions have no body
        resolved_param_types: None,
        resolved_return_type: None,
        resolved_function_type: None,
        assumptions: Vec::new(),
        span: None,
    });

    // Add to program declarations
    program.declarations.push(Item::Callable(function_id));

    function_id
}

/// Generate a setter function for a field  
fn generate_setter_function(
    program: &mut Program,
    function_name: &str,
    table_name: &str,
    field: &TableField,
) -> FunctionId {
    // Create the generic parameter T for the table type
    let generic_param_id = program.generic_params.alloc(GenericParam {
        name: Identifier {
            name: table_name.to_string(),
            span: None,
        },
        span: None,
    });

    // Create Row<T> type for both parameter and return type
    let row_base_type = create_identifier_type(program, "Row");
    let table_arg_type = create_identifier_type(program, table_name);
    let row_type = create_generic_type(program, row_base_type, vec![table_arg_type]);

    // Create parameters: (row: Row<TableName>, value: FieldType)
    let row_param_id = program.params.alloc(Parameter {
        name: Identifier {
            name: "row".to_string(),
            span: None,
        },
        ty: row_type,
        resolved_type: None,
        span: None,
    });

    let value_param_id = program.params.alloc(Parameter {
        name: Identifier {
            name: "value".to_string(),
            span: None,
        },
        ty: field.ty,
        resolved_type: None,
        span: None,
    });

    // Return type is Row<TableName>
    let return_row_base = create_identifier_type(program, "Row");
    let return_table_arg = create_identifier_type(program, table_name);
    let return_row_type = create_generic_type(program, return_row_base, vec![return_table_arg]);

    // Create the function declaration
    let function_id = program.functions.alloc(CallableDecl {
        kind: CallableKind::Function,
        name: Identifier {
            name: function_name.to_string(),
            span: None,
        },
        decorators: vec![
            Decorator {
                name: Identifier {
                    name: "generated".to_string(),
                    span: None,
                },
                span: None,
            },
            Decorator {
                name: Identifier {
                    name: "intrinsic".to_string(),
                    span: None,
                },
                span: None,
            },
        ],
        generic_params: vec![generic_param_id],
        params: vec![row_param_id, value_param_id],
        return_type: Some(return_row_type),
        body: None, // Intrinsic functions have no body
        resolved_param_types: None,
        resolved_return_type: None,
        resolved_function_type: None,
        assumptions: Vec::new(),
        span: None,
    });

    // Add to program declarations
    program.declarations.push(Item::Callable(function_id));

    function_id
}

/// Helper to create a named type AST node
fn create_identifier_type(program: &mut Program, name: &str) -> AstTypeId {
    program.types.alloc(AstType::Named {
        name: Identifier {
            name: name.to_string(),
            span: None,
        },
        resolved_type: None,
    })
}

/// Helper to create a generic type AST node
fn create_generic_type(program: &mut Program, base: AstTypeId, args: Vec<AstTypeId>) -> AstTypeId {
    // Get base name from the base type
    let base_name = match &program.types[base] {
        AstType::Named { name, .. } => name.clone(),
        _ => panic!("Expected named type for generic base"),
    };

    program.types.alloc(AstType::Generic {
        base: base_name,
        args,
        resolved_base_type: None,
        span: None,
    })
}
