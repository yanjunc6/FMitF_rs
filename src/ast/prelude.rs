use id_arena::Arena;

use crate::ast::*;

// ============================================================================
// --- Default Program
// ============================================================================

impl Default for Program {
    fn default() -> Self {
        let mut program = Program {
            declarations: Vec::new(),
            functions: Arena::new(),
            type_decls: Arena::new(),
            const_decls: Arena::new(),
            table_decls: Arena::new(),
            var_decls: Arena::new(),
            params: Arena::new(),
            generic_params: Arena::new(),
            types: Arena::new(),
            statements: Arena::new(),
            expressions: Arena::new(),
            blocks: Arena::new(),
        };

        // Add built-in types to the program
        let intrinsic_decorator = Decorator {
            name: Identifier {
                name: "intrinsic".to_string(),
                span: None,
                resolved: None,
                resolved_type: None,
            },
            span: None,
        };

        // Add built-in types: int, bool, string, float
        for type_name in &["int", "bool", "string", "float"] {
            let type_id = program.type_decls.alloc(TypeDecl {
                decorators: vec![intrinsic_decorator.clone()],
                name: Identifier {
                    name: type_name.to_string(),
                    span: None,
                    resolved: None,
                    resolved_type: None,
                },
                generic_params: Vec::new(),
                span: None,
            });
            program.declarations.push(Declaration::Type(type_id));
        }

        // Add generic List<T> type
        let list_generic_param_id = program.generic_params.alloc(GenericParam {
            name: Identifier {
                name: "T".to_string(),
                span: None,
                resolved: None,
                resolved_type: None,
            },
            span: None,
        });

        let list_type_id = program.type_decls.alloc(TypeDecl {
            decorators: vec![intrinsic_decorator.clone()],
            name: Identifier {
                name: "List".to_string(),
                span: None,
                resolved: None,
                resolved_type: None,
            },
            generic_params: vec![list_generic_param_id],
            span: None,
        });
        program.declarations.push(Declaration::Type(list_type_id));

        // Add built-in arithmetic operators
        let arithmetic_ops = vec![
            ("+", vec!["int", "int"], "int"),
            ("-", vec!["int", "int"], "int"),
            ("*", vec!["int", "int"], "int"),
            ("/", vec!["int", "int"], "int"),
            ("%", vec!["int", "int"], "int"),
            ("+", vec!["float", "float"], "float"),
            ("-", vec!["float", "float"], "float"),
            ("*", vec!["float", "float"], "float"),
            ("/", vec!["float", "float"], "float"),
        ];

        for (op, params, ret) in arithmetic_ops {
            add_builtin_operator(&mut program, op, params, ret);
        }

        // Add prefix operators
        add_builtin_prefix_operator(&mut program, "-", "int", "int");
        add_builtin_prefix_operator(&mut program, "-", "float", "float");
        add_builtin_prefix_operator(&mut program, "!", "bool", "bool");

        // Add comparison operators
        let comparison_ops = vec![
            ("==", vec!["int", "int"], "bool"),
            ("!=", vec!["int", "int"], "bool"),
            ("<", vec!["int", "int"], "bool"),
            ("<=", vec!["int", "int"], "bool"),
            (">", vec!["int", "int"], "bool"),
            (">=", vec!["int", "int"], "bool"),
            ("==", vec!["bool", "bool"], "bool"),
            ("!=", vec!["bool", "bool"], "bool"),
            ("==", vec!["string", "string"], "bool"),
            ("!=", vec!["string", "string"], "bool"),
            ("==", vec!["float", "float"], "bool"),
            ("!=", vec!["float", "float"], "bool"),
            ("<", vec!["float", "float"], "bool"),
            ("<=", vec!["float", "float"], "bool"),
            (">", vec!["float", "float"], "bool"),
            (">=", vec!["float", "float"], "bool"),
        ];

        for (op, params, ret) in comparison_ops {
            add_builtin_operator(&mut program, op, params, ret);
        }

        // Add logical operators
        add_builtin_operator(&mut program, "&&", vec!["bool", "bool"], "bool");
        add_builtin_operator(&mut program, "||", vec!["bool", "bool"], "bool");

        // Add string concatenation
        add_builtin_operator(&mut program, "+", vec!["string", "string"], "string");

        // Add built-in functions
        add_map_function(&mut program);
        add_filter_function(&mut program);
        add_reduce_function(&mut program);
        add_length_function(&mut program);

        program
    }
}

fn add_builtin_operator(
    program: &mut Program,
    op: &str,
    param_types: Vec<&str>,
    return_type: &str,
) {
    let mut params = Vec::new();
    for (i, param_type) in param_types.iter().enumerate() {
        let type_id = program.types.alloc(Type::Named(Identifier {
            name: param_type.to_string(),
            span: None,
            resolved: None,
            resolved_type: None,
        }));

        let param_id = program.params.alloc(Parameter {
            name: Identifier {
                name: format!("arg{}", i),
                span: None,
                resolved: None,
                resolved_type: None,
            },
            ty: type_id,
            span: None,
        });
        params.push(param_id);
    }

    let return_type_id = program.types.alloc(Type::Named(Identifier {
        name: return_type.to_string(),
        span: None,
        resolved: None,
        resolved_type: None,
    }));

    let func_id = program.functions.alloc(CallableDecl {
        decorators: vec![
            Decorator {
                name: Identifier {
                    name: "intrinsic".to_string(),
                    span: None,
                    resolved: None,
                    resolved_type: None,
                },
                span: None,
            },
            Decorator {
                name: Identifier {
                    name: "infix".to_string(),
                    span: None,
                    resolved: None,
                    resolved_type: None,
                },
                span: None,
            },
        ],
        kind: CallableKind::Operator,
        name: CallableName::Operator(Spanned {
            value: op.to_string(),
            span: None,
        }),
        generic_params: Vec::new(),
        params,
        return_type: Some(return_type_id),
        assumptions: Vec::new(),
        body: None,
        span: None,
    });

    program.declarations.push(Declaration::Callable(func_id));
}

fn add_builtin_prefix_operator(
    program: &mut Program,
    op: &str,
    param_type: &str,
    return_type: &str,
) {
    let type_id = program.types.alloc(Type::Named(Identifier {
        name: param_type.to_string(),
        span: None,
        resolved: None,
        resolved_type: None,
    }));

    let param_id = program.params.alloc(Parameter {
        name: Identifier {
            name: "arg".to_string(),
            span: None,
            resolved: None,
            resolved_type: None,
        },
        ty: type_id,
        span: None,
    });

    let return_type_id = program.types.alloc(Type::Named(Identifier {
        name: return_type.to_string(),
        span: None,
        resolved: None,
        resolved_type: None,
    }));

    let func_id = program.functions.alloc(CallableDecl {
        decorators: vec![
            Decorator {
                name: Identifier {
                    name: "intrinsic".to_string(),
                    span: None,
                    resolved: None,
                    resolved_type: None,
                },
                span: None,
            },
            Decorator {
                name: Identifier {
                    name: "prefix".to_string(),
                    span: None,
                    resolved: None,
                    resolved_type: None,
                },
                span: None,
            },
        ],
        kind: CallableKind::Operator,
        name: CallableName::Operator(Spanned {
            value: op.to_string(),
            span: None,
        }),
        generic_params: Vec::new(),
        params: vec![param_id],
        return_type: Some(return_type_id),
        assumptions: Vec::new(),
        body: None,
        span: None,
    });

    program.declarations.push(Declaration::Callable(func_id));
}

// Helper function to create List<T> type
fn create_list_type(program: &mut Program, element_type_name: &str) -> TypeId {
    let element_type_id = program.types.alloc(Type::Named(Identifier {
        name: element_type_name.to_string(),
        span: None,
        resolved: None,
        resolved_type: None,
    }));

    program.types.alloc(Type::Generic {
        base: Identifier {
            name: "List".to_string(),
            span: None,
            resolved: None,
            resolved_type: None,
        },
        args: vec![element_type_id],
        span: None,
    })
}

// Helper function to create function type (T) -> U
fn create_function_type(
    program: &mut Program,
    param_types: Vec<&str>,
    return_type: &str,
) -> TypeId {
    let mut param_type_ids = Vec::new();
    for param_type in param_types {
        let param_type_id = program.types.alloc(Type::Named(Identifier {
            name: param_type.to_string(),
            span: None,
            resolved: None,
            resolved_type: None,
        }));
        param_type_ids.push(param_type_id);
    }

    let return_type_id = program.types.alloc(Type::Named(Identifier {
        name: return_type.to_string(),
        span: None,
        resolved: None,
        resolved_type: None,
    }));

    program.types.alloc(Type::Function {
        params: param_type_ids,
        return_type: return_type_id,
        span: None,
    })
}

// Add map<T, U>(list: List<T>, func: (T) -> U) -> List<U>
fn add_map_function(program: &mut Program) {
    // Create generic parameters T and U
    let t_param = program.generic_params.alloc(GenericParam {
        name: Identifier {
            name: "T".to_string(),
            span: None,
            resolved: None,
            resolved_type: None,
        },
        span: None,
    });

    let u_param = program.generic_params.alloc(GenericParam {
        name: Identifier {
            name: "U".to_string(),
            span: None,
            resolved: None,
            resolved_type: None,
        },
        span: None,
    });

    // Create List<T> type
    let list_t_type = create_list_type(program, "T");

    // Create function type (T) -> U
    let func_type = create_function_type(program, vec!["T"], "U");

    // Create List<U> return type
    let list_u_type = create_list_type(program, "U");

    // Create parameters
    let list_param = program.params.alloc(Parameter {
        name: Identifier {
            name: "list".to_string(),
            span: None,
            resolved: None,
            resolved_type: None,
        },
        ty: list_t_type,
        span: None,
    });

    let func_param = program.params.alloc(Parameter {
        name: Identifier {
            name: "func".to_string(),
            span: None,
            resolved: None,
            resolved_type: None,
        },
        ty: func_type,
        span: None,
    });

    // Create function declaration
    let func_id = program.functions.alloc(CallableDecl {
        decorators: vec![Decorator {
            name: Identifier {
                name: "intrinsic".to_string(),
                span: None,
                resolved: None,
                resolved_type: None,
            },
            span: None,
        }],
        kind: CallableKind::Function,
        name: CallableName::Identifier(Identifier {
            name: "map".to_string(),
            span: None,
            resolved: None,
            resolved_type: None,
        }),
        generic_params: vec![t_param, u_param],
        params: vec![list_param, func_param],
        return_type: Some(list_u_type),
        assumptions: Vec::new(),
        body: None,
        span: None,
    });

    program.declarations.push(Declaration::Callable(func_id));
}

// Add filter<T>(list: List<T>, predicate: (T) -> bool) -> List<T>
fn add_filter_function(program: &mut Program) {
    // Create generic parameter T
    let t_param = program.generic_params.alloc(GenericParam {
        name: Identifier {
            name: "T".to_string(),
            span: None,
            resolved: None,
            resolved_type: None,
        },
        span: None,
    });

    // Create List<T> type
    let list_t_type = create_list_type(program, "T");

    // Create function type (T) -> bool
    let predicate_type = create_function_type(program, vec!["T"], "bool");

    // Create parameters
    let list_param = program.params.alloc(Parameter {
        name: Identifier {
            name: "list".to_string(),
            span: None,
            resolved: None,
            resolved_type: None,
        },
        ty: list_t_type,
        span: None,
    });

    let predicate_param = program.params.alloc(Parameter {
        name: Identifier {
            name: "predicate".to_string(),
            span: None,
            resolved: None,
            resolved_type: None,
        },
        ty: predicate_type,
        span: None,
    });

    // Create return type List<T>
    let return_type = create_list_type(program, "T");

    // Create function declaration
    let func_id = program.functions.alloc(CallableDecl {
        decorators: vec![Decorator {
            name: Identifier {
                name: "intrinsic".to_string(),
                span: None,
                resolved: None,
                resolved_type: None,
            },
            span: None,
        }],
        kind: CallableKind::Function,
        name: CallableName::Identifier(Identifier {
            name: "filter".to_string(),
            span: None,
            resolved: None,
            resolved_type: None,
        }),
        generic_params: vec![t_param],
        params: vec![list_param, predicate_param],
        return_type: Some(return_type),
        assumptions: Vec::new(),
        body: None,
        span: None,
    });

    program.declarations.push(Declaration::Callable(func_id));
}

// Add reduce<T, U>(list: List<T>, func: (U, T) -> U, initial: U) -> U
fn add_reduce_function(program: &mut Program) {
    // Create generic parameters T and U
    let t_param = program.generic_params.alloc(GenericParam {
        name: Identifier {
            name: "T".to_string(),
            span: None,
            resolved: None,
            resolved_type: None,
        },
        span: None,
    });

    let u_param = program.generic_params.alloc(GenericParam {
        name: Identifier {
            name: "U".to_string(),
            span: None,
            resolved: None,
            resolved_type: None,
        },
        span: None,
    });

    // Create List<T> type
    let list_t_type = create_list_type(program, "T");

    // Create function type (U, T) -> U
    let func_type = create_function_type(program, vec!["U", "T"], "U");

    // Create U type
    let u_type = program.types.alloc(Type::Named(Identifier {
        name: "U".to_string(),
        span: None,
        resolved: None,
        resolved_type: None,
    }));

    // Create parameters
    let list_param = program.params.alloc(Parameter {
        name: Identifier {
            name: "list".to_string(),
            span: None,
            resolved: None,
            resolved_type: None,
        },
        ty: list_t_type,
        span: None,
    });

    let func_param = program.params.alloc(Parameter {
        name: Identifier {
            name: "func".to_string(),
            span: None,
            resolved: None,
            resolved_type: None,
        },
        ty: func_type,
        span: None,
    });

    let initial_param = program.params.alloc(Parameter {
        name: Identifier {
            name: "initial".to_string(),
            span: None,
            resolved: None,
            resolved_type: None,
        },
        ty: u_type,
        span: None,
    });

    // Create return type U
    let return_type = program.types.alloc(Type::Named(Identifier {
        name: "U".to_string(),
        span: None,
        resolved: None,
        resolved_type: None,
    }));

    // Create function declaration
    let func_id = program.functions.alloc(CallableDecl {
        decorators: vec![Decorator {
            name: Identifier {
                name: "intrinsic".to_string(),
                span: None,
                resolved: None,
                resolved_type: None,
            },
            span: None,
        }],
        kind: CallableKind::Function,
        name: CallableName::Identifier(Identifier {
            name: "reduce".to_string(),
            span: None,
            resolved: None,
            resolved_type: None,
        }),
        generic_params: vec![t_param, u_param],
        params: vec![list_param, func_param, initial_param],
        return_type: Some(return_type),
        assumptions: Vec::new(),
        body: None,
        span: None,
    });

    program.declarations.push(Declaration::Callable(func_id));
}

// Add length<T>(list: List<T>) -> int
fn add_length_function(program: &mut Program) {
    // Create generic parameter T
    let t_param = program.generic_params.alloc(GenericParam {
        name: Identifier {
            name: "T".to_string(),
            span: None,
            resolved: None,
            resolved_type: None,
        },
        span: None,
    });

    // Create List<T> type
    let list_t_type = create_list_type(program, "T");

    // Create parameter
    let list_param = program.params.alloc(Parameter {
        name: Identifier {
            name: "list".to_string(),
            span: None,
            resolved: None,
            resolved_type: None,
        },
        ty: list_t_type,
        span: None,
    });

    // Create return type int
    let return_type = program.types.alloc(Type::Named(Identifier {
        name: "int".to_string(),
        span: None,
        resolved: None,
        resolved_type: None,
    }));

    // Create function declaration
    let func_id = program.functions.alloc(CallableDecl {
        decorators: vec![Decorator {
            name: Identifier {
                name: "intrinsic".to_string(),
                span: None,
                resolved: None,
                resolved_type: None,
            },
            span: None,
        }],
        kind: CallableKind::Function,
        name: CallableName::Identifier(Identifier {
            name: "length".to_string(),
            span: None,
            resolved: None,
            resolved_type: None,
        }),
        generic_params: vec![t_param],
        params: vec![list_param],
        return_type: Some(return_type),
        assumptions: Vec::new(),
        body: None,
        span: None,
    });

    program.declarations.push(Declaration::Callable(func_id));
}
