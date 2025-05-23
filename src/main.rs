mod ast;
mod parser;
mod printer;

use crate::parser::parse_program;
use crate::printer::print_program;

fn main() {
    let source_code: &'static str = r#"
nodes {
    NodeA,
    NodeB
}
table Customers on NodeA {
    int customerID;
    float customerPoints;
};
void applyPoints(int threshold) {
    hop on NodeA {
        Customers[customerID: 123].customerPoints = 10.0;
    }
    hop on NodeB {
        if (threshold > 100) {
            // some statements
        } else {
            // other statements
        }
    }
    
}
"#;

    let program = parse_program(source_code);
    // Now 'program' is your final AST with references to nodes/tables, etc.
    println!("AST built successfully!");
    // You could inspect `program.functions`, `program.tables`, etc.
    print_program(&program);
}
